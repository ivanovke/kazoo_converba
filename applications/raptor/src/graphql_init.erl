%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(graphql_init).

-export([start_link/0
        ,mapping_rules/0
        ,setup_root/0
        ]).

-include("raptor.hrl").

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
    PrivDir = code:priv_dir(?APP),
    {'ok', SchemaData} = file:read_file(filename:join(PrivDir, "raptor.graphql")),
    'ok' = graphql:load_schema(mapping_rules(), SchemaData),
    'ok' = setup_root(),
    'ok' = graphql:validate_schema(),
    'ok'.

-spec mapping_rules() -> map().
mapping_rules() ->
    #{'interfaces' => #{'default' => 'gql_core_type'}
     ,'enums' => #{'default'   => 'gql_core_enum'}
     ,'objects' => #{'Account' => 'gql_core_account'

                    ,'Query' => 'gql_core_query'
                    ,'default' => 'gql_core_object'
                    }
     %% ,'unions' => #{'default' => 'gql_core_type'}
     %% ,'scalars' => #{'default' => 'gql_core_scalar'}
     }.

-spec setup_root() -> any().
setup_root() ->
    Root = {'root'
           ,#{'query' => 'Query'
             ,'interfaces' => ['Node']
             }
           },
    'ok' = graphql:insert_schema_definition(Root),
    'ok'.
