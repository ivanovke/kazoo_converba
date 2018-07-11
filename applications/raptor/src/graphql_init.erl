%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(graphql_init).

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

mapping_rules() ->
    #{'scalars' => #{'default' => 'gql_core_scalar'}
     ,'interfaces' => #{'default' => 'gql_core_type'}
     ,'unions' => #{'default' => 'gql_core_type'}
     ,'enums' => #{'Episode' => 'gql_core_enum'
                  ,'default'   => 'gql_core_enum'
                  }
     ,'objects' => #{'Account' => 'gql_core_account'

                    ,'Query' => 'gql_core_query'
                    ,'Mutation' => 'gql_core_mutation'
                    ,'default' => 'gql_core_object'
                    }
     }.

setup_root() ->
    Root = {'root'
           ,#{'query' => 'Query'
             ,'mutation' => 'Mutation'
             ,'interfaces' => ['Node']
             }
           },
    'ok' = graphql:insert_schema_definition(Root),
    'ok'.
