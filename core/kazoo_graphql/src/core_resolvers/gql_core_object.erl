%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(gql_core_object).

-export([execute/4
        ]).

-include("kz_graphql.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec execute(any(), any(), any(), any()) -> any().
execute(_Context, JObj, <<"callRestriction">> = Field, _Args) ->
    Key = kgql_utils:normalize_key(Field),
    ?DEV_LOG("CallRestriction"),
    {'ok', maps:get(Key, JObj, 'null')};
execute(_Context, JObj, Field, _Args) when is_map(JObj) ->
    Key = kgql_utils:normalize_key(Field),
    ?DEV_LOG("core object normalized key: ~p", [Key]),
    {'ok', maps:get(Key, JObj, 'null')};
execute(_Context, Obj, _Field, _Args) ->
    ?DEV_LOG("core object: field ~p is not map", [_Field]),
    {'error', {'not_map_object', Obj}}.
