%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_graphql_init).

-export([start_link/0
        ,load_schema/0
        ,setup_root/0
        ]).

-include("kz_graphql.hrl").

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    load_schema(),
    'ignore'.

-spec load_schema() -> 'ok'.
load_schema() ->
    graphql_schema:reset(),
    AppResolvers = ['gql_account'
                   ],
    Resolvers = [<<"gql_core_enum">>
                ,<<"gql_core_object">>
                ,<<"gql_core_scalar">>
                ,<<"gql_root_queries">>
                 | AppResolvers
                ],
    lager:info("loading GraphQL Schema for: ~p", [AppResolvers]),
    SchemaData = read_schema_files(Resolvers),
    MappingRules = inject_core(AppResolvers),

    try graphql:load_schema(MappingRules, SchemaData) of
        'ok' -> setup_and_validate();
        {'error', {error, {already_exists, _Id}=Error}} ->
            lager:error("Id ~p is already defined in GraphQL Schema", []),
            exit(Error);
        {'error', Error} ->
            lager:error("failed to GraphQL Schemas: ~p", [Error]),
            exit(Error)
    catch
        'error':Error ->
            lager:error("failed to GraphQL Schemas: ~p", [Error]),
            exit(Error)
    end.

setup_and_validate() ->
    case setup_root() of
        'ok' ->
            ?DEV_LOG("yello"),
            try graphql:validate_schema()
            catch
                _T:Error ->
                    ?DEV_LOG("GraphQL Schema validation failed: ~p:~p", [_T, Error]),
                    exit(Error)
            end;
        {'error', Error} ->
            lager:error("inserting GraphQL root schema failed: ~p", [Error]),
            exit(Error)
    end.

read_schema_files(Resolvers) ->
    PrivDir = code:priv_dir(?APP),
    kz_term:to_binary([read_schema(PrivDir, Resolver) || Resolver <- Resolvers]).

read_schema(PrivDir, Resolver) ->
    File = filename:join([PrivDir, <<"schemas">>, <<(kz_term:to_binary(Resolver))/binary, ".graphql">>]),
    case file:read_file(File) of
        {'ok', SchemaData} ->
            <<SchemaData/binary, "\n">>;
        {'error', _Reason}=Error ->
            lager:error("failed to read GraphQL schema file ~s: ~p", [File, _Reason]),
            exit(Error)
    end.

inject_core(Resolvers) ->
    ResolversMapping = [Resolver:mapping_rules() || Resolver <- Resolvers],
    merge_mapping_rules([core_mapping_rules() | ResolversMapping], #{}).

-spec merge_mapping_rules(map(), map()) -> map().
merge_mapping_rules([], Acc) ->
    Acc;
merge_mapping_rules([ResolverRules | Other], MappingRules) ->
    NewAcc = maps:fold(fun(K, V, Acc) -> Acc#{K => maps:merge(V, maps:get(K, Acc, #{}))} end
                      ,MappingRules
                      ,ResolverRules
                      ),
    merge_mapping_rules(Other, NewAcc).

-spec core_mapping_rules() -> map().
core_mapping_rules() ->
    #{'interfaces' => #{'default' => 'gql_core_type'}
     ,'enums' => #{'default'   => 'gql_core_enum'}
     ,'objects' => #{'Query' => 'gql_core_query'
                    ,'default' => 'gql_core_object'
                    %% ,'Mutation' => 'gql_core_mutation'
                    }
     %% ,'unions' => #{'default' => 'gql_core_type'}
     ,'scalars' => #{'default' => 'gql_core_scalar'}
     }.

-spec setup_root() -> any().
setup_root() ->
    Root = {'root'
           ,#{'query' => 'Query'
             ,'interfaces' => ['Node']
             %% ,mutation => 'Mutation'
             }
           },
    graphql:insert_schema_definition(Root).
