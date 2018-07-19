%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(gql_core_account).

-export([execute/1 %% type resolution
        ,execute/4 %% object resolution
        %% ,input/2, output/2 %% scalar values coercing

        ,mapping_rules/0
        ]).

-include("kz_graphql.hrl").


%%------------------------------------------------------------------------------
%% @doc Mapping this module to types.
%% @end
%%------------------------------------------------------------------------------
-spec mapping_rules() -> map().
mapping_rules() ->
    #{'interfaces' => #{'Account' => 'gql_core_account'}
     ,'objects' => #{'Account' => 'gql_core_account'}
     }.

%%------------------------------------------------------------------------------
%% @doc Type materializer for this resolver.
%% @end
%%------------------------------------------------------------------------------
-spec execute(any()) -> {'ok', any()} | {'error', any()}.
execute(#{'$type' := <<"account">>}) ->
    ?DEV_LOG("type account"),
    {'ok', 'Account'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec execute(any(), any(), any(), any()) -> any().
execute(_Context, #{'object' := JObj}, Field, _Args) ->
    Key = kgql_utils:normalize_key(Field),
    {'ok', maps:get(Key, JObj, 'null')}.

