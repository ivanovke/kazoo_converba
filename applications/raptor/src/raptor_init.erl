%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(raptor_init).

-export([start_link/0
        ]).

-include("raptor.hrl").

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    Dispatch = cowboy_router:compile(graphql_routes()),
    maybe_start_plaintext(Dispatch),
    'ignore'.

-spec graphql_routes() -> cowboy_router:routes().
graphql_routes() ->
    [{'_'
     ,[assets_path()
      ,graphql_path()
      ]
     }
    ].

assets_path() ->
    {"/___graphql/[...]", 'cowboy_static', {'priv_dir', ?APP, "graphiql"}}.

graphql_path() ->
    {"/", 'raptor_graphql_handler', {'priv_dir', ?APP, "graphiql/index.html"}}.

maybe_start_plaintext(Dispatch) ->
    Port = kapps_config:get_integer(?CONFIG_CAT, <<"port">>, 8001),
    ReqTimeout = kapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10 * ?MILLISECONDS_IN_SECOND),
    Workers = kapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 100),

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    try
        IP = kz_network_utils:get_binding_ip(),
        lager:info("trying to bind to address ~s port ~b", [inet:ntoa(IP), Port]),
        cowboy:start_clear('graphql_resource'
                          ,[{'ip', IP}
                           ,{'port', Port}
                           ,{'num_acceptors', Workers}
                           ]
                          ,#{'env' => #{'dispatch' => Dispatch
                                       ,'timeout' => ReqTimeout
                                       }
                            ,'stream_handlers' => ['cowboy_compress_h', 'cowboy_stream_h']
                             %% Bump the default limit of 8000 to 65536 to allow us to submit
                             %% slightly larger, human readable, query documents. The limit of
                             %% 65536 is chosen to allow us to have 8 times bigger documents
                             %% than the default where we hit the limit of 8000. If you are
                             %% hitting the bumped limit you should probably consider splitting
                             %% up your query document into two.
                             %%
                             %% Caveat: If you are testing on localhost you might not see the
                             %% max limit have any effect since the socket might make the entire
                             %% HTTP request available when cowboy does a gen_tcp:read(Socket, 0)
                             %% and will ignore the limit.
                            ,'max_request_line_length' => 65536

                             %% Bump the default limit of 4096 on Header lengths to 16384. The
                             %% problem is we will eventually get a very large document as a
                             %% referrer from GraphiQL and this will break the server side as it
                             %% has to process through that header
                            ,'max_header_value_length' => 16384
                            }
                          )
    of
        {'ok', _} ->
            lager:info("started plaintext GraphQL server");
        {'error', {'already_started', _P}} ->
            lager:info("already started plaintext GraphQL server at ~p", [_P])
    catch
        _E:_R ->
            lager:warning("crashed starting GraphQL server: ~s: ~p", [_E, _R])
    end.
