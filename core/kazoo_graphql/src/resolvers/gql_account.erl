%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(gql_account).

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
    #{'interfaces' => #{'Account' => 'gql_account'}
     ,'objects' => #{'Account' => 'gql_account'}
     }.

%%------------------------------------------------------------------------------
%% @doc Type materializer for this resolver.
%% @end
%%------------------------------------------------------------------------------
-spec execute(any()) -> {'ok', any()} | {'error', any()}.
execute(#{'$type' := <<"account">>}) ->
    ?DEV_LOG("$type map account"),
    {'ok', 'Account'};
execute(_So) ->
    ?DEV_LOG("type account so ~p", [_So]),
    {'ok', 'Account'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec execute(any(), any(), any(), any()) -> any().
execute(_Context, #{'id' := Id, 'object' := #{}}, <<"id">>, _Args) ->
    ?DEV_LOG("_Args ~p", [_Args]),
    {'ok', Id};
execute(_Context, #{'object' := JObj}, Field, _Args) ->
    Key = kgql_utils:normalize_key(Field),
    ?DEV_LOG("_Args ~p", [_Args]),
    ?DEV_LOG("field ~s(~s) from account object:~nValue ~p", [Field, Key, maps:get(Key, JObj, 'null')]),
    resolve_field(Field, maps:get(Key, JObj, 'null')).

resolve_field(<<"callRestriction">>, Value) ->
    kgql_utils:resolve_jobj_to_list(Value);
resolve_field(<<"formatters">>, Value) ->
    kgql_utils:resolve_jobj_to_list(Value, 'false', 'matchKey');
resolve_field(<<"musicOnHold">>, 'null') ->
    {'ok', 'null'};
resolve_field(<<"musicOnHold">>, Value) ->
    {'ok', maps:get(<<"media_id">>, Value, null)};
resolve_field(_Key, Value) ->
    {'ok', Value}.
