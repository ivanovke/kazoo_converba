%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Runs tests against the Crossbar API using all available PQC modules
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_runner).
-behaviour(proper_statem).

%% PropEr Statem callbacks
-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
         %% ,correct_parallel/0
        ]).

%% manual run
-export([run_counterexample/0, run_counterexample/1
        ,create_counterexample/1
        ,cleanup/0
        ,pqc_modules/0
        ]).

-include_lib("proper/include/proper.hrl").
-include("kazoo_proper.hrl").

-spec correct() -> any().
correct() ->
    _ = setup(),
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   try run_commands(?MODULE, Cmds) of
                       {History, Model, Result} ->
                           cleanup(),
                           ?WHENFAIL(output_failure(Model, Cmds, History, Result)
                                    ,aggregate(command_names(Cmds), Result =:= 'ok')
                                    )
                   catch
                       E:R ->
                           ST = erlang:get_stacktrace(),
                           lager:info("~s: ~p~n~p~n~p~n", [E, R, ST, Cmds])
                   end
               end
              )
           ).

output_failure(Model, Cmds, [], Result) ->
    io:format('user'
             ,"Final Model:~n~p~n~nFailing Cmds:~n~p~nResult: ~p~n"
             ,[Model, Cmds, Result]
             );
output_failure(Model, Cmds, History, Result) ->
    io:format('user'
             ,"Final Model:~n~p~n~nFailing Cmds:~n~p~nResult: ~p~nFailing command: ~p~n"
             ,[Model, zip(Cmds, History), Result
              ,lists:nth(length(History), Cmds)
              ]
             ).

setup() ->
    TestId = kz_binary:rand_hex(4),
    kz_util:put_callid(TestId),

    lager:debug("cleaning up before setup"),
    catch cleanup(),

    _ = setup_system(),

    lager:info("SETUP COMPLETE").

setup_system() ->
    {'ok', _} = kapps_controller:start_app('crossbar').

%% -spec correct_parallel() -> any().
%% correct_parallel() ->
%%     _ = setup(),
%%     ?FORALL(Cmds
%%            ,parallel_commands(?MODULE)
%%            ,?TRAPEXIT(
%%                begin
%%                    {Seq, Par, Result} = run_parallel_commands(?MODULE, Cmds),

%%                    ?WHENFAIL(lager:info("R: ~p~nS: ~p~nP: ~p~n", [Result, Seq, Par])
%%                             ,aggregate(command_names(Cmds), Result =:= 'ok')
%%                             )
%%                end
%%               )
%%            ).

-spec initial_state() -> pqc_kazoo_model:model().
initial_state() ->
    API = pqc_cb_api:authenticate(),
    ?INFO("state initialized to ~p", [API]),
    init_system(),
    init_modules(pqc_modules()),
    pqc_kazoo_model:new(API).

init_system() ->
    _ = kapps_controller:start_app('crossbar'),
    'ok'.

init_modules(Modules) ->
    _ = [M:init() || M <- Modules,
                     kz_module:is_exported(M, 'init', 0)
        ],
    'ok'.

-spec command(pqc_kazoo_model:model()) -> proper_types:type().
command(Model) ->
    frequency(lists:foldl(fun(M, Cs) -> add_api_calls(M, Cs, Model) end, [], pqc_modules())).

add_api_calls(Module, Acc, Model) ->
    case Module:api_calls(Model) of
        [{'call', _, _, _}|_]=Calls ->
            [{10, Call} || Call <- Calls] ++ Acc;
        [{N, {'call', _, _, _}}|_]=Freqs when is_integer(N) ->
            Freqs ++ Acc
    end.

-spec pqc_modules() -> kz_term:atoms().
pqc_modules() ->
    {'ok', Modules} = application:get_key(?APP, 'modules'),
    [Module || Module <- Modules,
               is_pqc_module(kz_term:to_binary(Module))
                   andalso kz_module:is_exported(Module, 'api_calls', 1)
    ].

-spec is_pqc_module(kz_term:ne_binary()) -> boolean().
is_pqc_module(<<"pqc_", _/binary>>) -> 'true';
is_pqc_module(_) -> 'false'.

-spec precondition(pqc_kazoo_model:model(), api_call()) -> 'true'.
precondition(_Model, {'call', ?MODULE, _F, _A}) -> 'true';
precondition(Model, {'call', Module, _F, _A}=Call) ->
    case kz_module:is_exported(Module, 'precondition', 2) of
        'true' -> Module:precondition(Model, Call);
        'false' -> 'true'
    end.

-spec postcondition(pqc_kazoo_model:model(), api_call(), api_response()) ->
                           boolean().
postcondition(_Model, {'call', ?MODULE, _F, _As}, Result) ->
    Result;
postcondition(Model, {'call', Module, _F, _As}=Call, Result) ->
    Module:check_response(Model, Call, Result).

-spec next_state(pqc_kazoo_model:model(), api_response(), api_call()) ->
                        pqc_kazoo_model:model().
next_state(Model, _Result, {'call', ?MODULE, _F, _As}) -> Model;
next_state(Model, Result, {'call', Module, _F, _As}=Call) ->
    Module:update_model(Model, Result, Call).

-spec cleanup() -> 'ok'.
cleanup() ->
    lists:foreach(fun cleanup_pqc/1, pqc_modules()),
    kt_cleanup:cleanup_soft_deletes(?KZ_ACCOUNTS_DB),
    lager:info("CLEANUP DONE").

cleanup_pqc(Module) ->
    cleanup_pqc(Module, kz_module:is_exported(Module, 'cleanup', 0)).

cleanup_pqc(_Module, 'false') -> 'ok';
cleanup_pqc(Module, 'true') ->
    Module:cleanup(),
    lager:debug("~s cleaned up", [Module]).

-spec create_counterexample(api_calls()) -> 'ok'.
create_counterexample(Calls) ->
    {_, Steps} = lists:foldr(fun call_to_step/2, {1, []}, Calls),
    run_counterexample([Steps]).

call_to_step(Call, {Var, Steps}) ->
    {Var+1
    ,[{'set', {'var', Var}, Call} | Steps]
    }.

-spec run_counterexample() -> 'ok'.
run_counterexample() ->
    run_counterexample(proper:counterexample()).

-spec run_counterexample('undefined' | list()) -> 'ok'.
run_counterexample('undefined') ->
    io:format("no counterexample, run proper qc~n");
run_counterexample(Commands) when is_list(Commands) ->
    _ = setup(),
    Model = initial_state(),
    lager:info("initial state: ~p~n", [Model]),
    case run_counterexample(Model, Commands) of
        'ok' -> io:format("FAILURE~n");
        {_Step, _FinalModel, _QCVars} ->
            io:format("FINAL STATE: ~s~nSUCCESS~n", [printable_model(_FinalModel)])
    end,
    cleanup().

run_counterexample(Model, [{Seq, Threads}]) ->
    Steps = lists:usort(fun sort_steps/2, Seq ++ lists:flatten(Threads)),
    lists:foldl(fun run_step/2, {0, Model, #{}}, Steps);
run_counterexample(Model, [Steps]) ->
    lists:foldl(fun run_step/2, {0, Model, #{}}, Steps).

sort_steps({'set', Var1, _Call1}, {'set', Var2, _Call2}) ->
    Var1 < Var2.

-type run_acc() :: {non_neg_integer(), pqc_kazoo_model:model(), #{integer() => any()}}.
-spec run_step(tuple(), run_acc() | 'ok') -> run_acc() | 'ok'.
run_step(_, 'ok') -> 'ok';
run_step({'set', Var, {'call', M, F, [_OldModel | As]}}, {Step, Model, QCVars}=Acc) ->
    Args = proper_symb:eval(QCVars, [Model | As]),

    lager:info("~p: ~p:~p(~s): ", [Step, M, F, printable_args(Args)]),
    try apply(M, F, Args) of
        SUTResponse ->
            eval_step(Var, M, F, Args, Acc, SUTResponse)
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:info("~n~s: ~p~n~p~n~p~n"
                      ,[_E, _R, ST, Model]
                      )
    end.

eval_step(Var, M, F, Args, {Step, Model, QCVars}, SUTResponse) ->
    lager:info("sut response: ~s~n", [printable_sut_response(SUTResponse)]),

    try postcondition(Model, {'call', M, F, Args}, SUTResponse) of
        'true' ->
            {Step+1
            ,next_state(Model, SUTResponse, {'call', M, F, Args})
            ,QCVars#{Var => SUTResponse}
            };
        'false' ->
            lager:info("postcondition failed~n", [])
    catch
        'throw':Error ->
            lager:info("postcondition crashed: ~p~n", [Error])
    end.

printable_args([Model | Args]) ->
    kz_binary:join([printable_model(Model) | [printable_arg(Arg) || Arg <- Args]], <<", ">>).

printable_arg(Tuple) when is_tuple(Tuple) ->
    [Type, Arg | _]=tuple_to_list(Tuple),
    [kz_term:to_binary(Type), ":", kz_term:to_binary(Arg)];
printable_arg(List) when is_list(List) ->
    [printable_arg(El) || El <- List];
printable_arg(Bin) when is_binary(Bin) -> Bin.

printable_model(Model) ->
    pqc_kazoo_model:printable(Model).

printable_sut_response({'ok', _}=Resp) ->
    io_lib:format("~p", [Resp]);
printable_sut_response({'error', Code, Body}) ->
    io_lib:format("error: ~p: ~s", [Code, Body]);
printable_sut_response(Resp) ->
    case kz_json:is_json_object(Resp) of
        'true' -> kz_json:encode(Resp);
        'false' -> io_lib:format("~p", [Resp])
    end.
