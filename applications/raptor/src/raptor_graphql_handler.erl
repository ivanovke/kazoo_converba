%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc GraphQL resource.
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(raptor_graphql_handler).

-include("raptor.hrl").

%% Cowboy Handler Interface
-export([init/2]).

%% REST callbacks
-export([allowed_methods/2
        ,resource_exists/2
        ,content_types_provided/2
        ,content_types_accepted/2
        ,charsets_provided/2
        ]).

%% Data input/output callbacks
-export([from_json/2
        ,to_json/2
        ,to_html/2
        ]).

%%%=============================================================================
%%% Startup and shutdown of request
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initialize a REST request.
%% @end
%%------------------------------------------------------------------------------
-spec init(cowboy_req:req(), kz_term:proplist()) ->
                  {'cowboy_rest', cowboy_req:req(), raptor_context:context()}.
init(Req, {'priv_dir', _, _} = PrivFile) ->
    {'cowboy_rest', Req, #{'grapgql_ide_location' => PrivFile}}.

-spec allowed_methods(cowboy_req:req(), raptor_context:context()) ->
                             {http_methods() | 'stop', cowboy_req:req(), raptor_context:context()}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

-spec content_types_accepted(cowboy_req:req(), raptor_context:context()) ->
                                    {content_type_callbacks(), cowboy_req:req(), raptor_context:context()}.
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, 'from_json'}], Req, State}.

-type content_type_callbacks() :: [{{kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()}, atom()} |
                                   {kz_term:ne_binary(), atom()}
                                  ].
-spec content_types_provided(cowboy_req:req(), raptor_context:context()) ->
                                    {content_type_callbacks(), cowboy_req:req(), raptor_context:context()}.
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, 'to_json'}
    ,{{<<"text">>, <<"html">>, []}, 'to_html'}
     ], Req, State}.

-spec charsets_provided(cowboy_req:req(), raptor_context:context()) ->
                            {kz_term:ne_binaries() %% lowercase; case insensitive
                            ,cowboy_req:req()
                            ,raptor_context:context()
                            }.
charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

-spec resource_exists(cowboy_req:req(), raptor_context:context()) ->
                             {boolean(), cowboy_req:req(), raptor_context:context()}.
resource_exists(#{method := <<"GET">>} = Req, State) ->
    {true, Req, State};
resource_exists(#{method := <<"POST">>} = Req, State) ->
    {false, Req, State}.

-spec to_html(cowboy_req:req(), raptor_context:context()) ->
                     {iolist(), cowboy_req:req(), raptor_context:context()}.
to_html(Req, #{grapgql_ide_location := {'priv_dir', App, FileLocation}} = State) ->
    Filename = filename:join(code:priv_dir(App), FileLocation),
    case file:read_file(Filename) of
        {'ok', Data} ->
            {Data, Req, State};
        {'error', _Reason} ->
            lager:debug("failed to read graphql IDE index file: ~p", [_Reason]),
            error_response(500, <<"no IDE">>, Req, State)
    end.

-spec from_json(cowboy_req:req(), raptor_context:context()) ->
                     {iolist() | kz_term:ne_binary() | 'stop', cowboy_req:req(), raptor_context:context()}.
from_json(Req, State) ->
    json_request(Req, State).

-spec to_json(cowboy_req:req(), raptor_context:context()) ->
                     {iolist() | kz_term:ne_binary() | 'stop', cowboy_req:req(), raptor_context:context()}.
to_json(Req, State) ->
    json_request(Req, State).

-spec json_request(cowboy_req:req(), raptor_context:context()) ->
                          {iolist() | kz_term:ne_binary() | 'stop', cowboy_req:req(), raptor_context:context()}.
json_request(Req, State) ->
    case gather_graphql_request(Req) of
        {'error', Reason} ->
            error_response(400, Reason, Req, State);
        {'ok', Req2, Decoded} ->
            run_request(Decoded, Req2, State)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Run request.
%% @end
%%------------------------------------------------------------------------------
run_request(#{document := 'undefined'}, Req, State) ->
    error_response(400, <<"no query supplied">>, Req, State);
run_request(#{document := Doc} = ReqCtx, Req, State) ->
    case graphql:parse(Doc) of
        {'ok', AST} ->
            run_preprocess(ReqCtx#{document := AST}, Req, State);
        {'error', Reason} ->
            error_response(400, Reason, Req, State)
    end.

run_preprocess(#{document := AST} = ReqCtx, Req, State) ->
    try
        Elaborated = graphql:elaborate(AST),
        {'ok', #{fun_env := FunEnv
                ,ast := AST2
                }
        } = graphql:type_check(Elaborated),
        'ok' = graphql:validate(AST2),
        run_execute(ReqCtx#{document := AST2
                           ,fun_env => FunEnv
                           }
                   ,Req
                   ,State
                   )
    catch
        'throw':Err ->
            error_response(400, Err, Req, State)
    end.

run_execute(#{document := AST
             ,fun_env := FunEnv
             ,vars := Vars
             ,operation_name := OpName
             }, Req, State) ->
    Coerced = graphql:type_check_params(FunEnv, OpName, Vars),
    Ctx = #{params => Coerced
           ,operation_name => OpName
           },
    Response = graphql:execute(Ctx, AST),
    ResponseBody = term_to_json(Response),
    Req2 = cowboy_req:set_resp_body(ResponseBody, Req),
    Reply = cowboy_req:reply(200, Req2),
    {'stop', Reply, State}.

gather_graphql_request(Req) ->
    {'ok', Body, Req2} = cowboy_req:read_body(Req),
    Bindings = cowboy_req:bindings(Req2),
    try jsx:decode(Body, ['return_maps']) of
        JSON ->
            gather_graphql_request(Req2, JSON, Bindings)
    catch
        'error':'badarg' ->
            {'error', <<"invalid JSON body">>}
    end.

gather_graphql_request(Req, Body, Params) ->
    QueryDocument = document([Params, Body]),
    case variables([Params, Body]) of
        {'ok', Vars} ->
            Operation = operation_name([Params, Body]),
            {'ok', Req, #{document => QueryDocument
                         ,vars => Vars
                         ,operation_name => Operation
                         }
            };
        {'error', Reason} ->
            {'error', Reason}
    end.

document([#{<<"query">> := Q} | _]) -> Q;
document([_ | Next]) -> document(Next);
document([]) -> 'undefined'.

variables([#{<<"variables">> := Vars} | _]) when is_binary(Vars) ->
    try jsx:decode(Vars, ['return_maps']) of
        'null' -> {'ok', #{}};
        JSON when is_map(JSON) -> {'ok', JSON};
        _ -> {'error', <<"invalid variable JSON">>}
    catch
        'error':'badarg' ->
            {'error', <<"invalid variable JSON">>}
    end;
variables([#{<<"variables">> := #{}=Vars} | _]) ->
    {'ok', Vars};
variables([#{<<"variables">> := 'null'} | _]) ->
    {'ok', #{}};
variables([_ | Next]) ->
    variables(Next);
variables([]) ->
    {'ok', #{}}.

operation_name([#{<<"operationName">> := OpName} | _]) ->
    OpName;
operation_name([_ | Next]) ->
    operation_name(Next);
operation_name([]) ->
    'undefined'.

error_response(Code, Msg, Req, State) ->
    Formatted = iolist_to_binary(io_lib:format("~p", [Msg])),
    Err = #{type => 'error'
           ,message => Formatted
           },
    Body = jsx:encode(#{errors => [Err]}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(Code, Req2),
    {'stop', Reply, State}.

term_to_json(Term) ->
    jsx:encode(fixup(Term)).

%% Ground types
fixup(Term) when is_number(Term) -> Term;
fixup(Term) when is_atom(Term) -> Term;
fixup(Term) when is_binary(Term) -> Term;
%% Compound types
fixup(Term) when is_list(Term) ->
    [fixup(T) || T <- Term];
fixup(Term) when is_map(Term) ->
    KVs = maps:to_list(Term),
    maps:from_list([{fixup_key(K), fixup(V)} || {K, V} <- KVs]);
fixup(Term) ->
    %% Every other term is transformed into a binary value
    iolist_to_binary(io_lib:format("~p", [Term])).

fixup_key(Term) ->
    case fixup(Term) of
        T when is_binary(T) -> T;
        T -> iolist_to_binary(io_lib:format("~p", [T]))
    end.
