%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(gql_core_account).

-export([execute/4
        ]).

-include("raptor.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec execute(any(), any(), any(), any()) -> any().
execute(_Context, #{'object' := JObj}, Field, _Args) ->
    ?DEV_LOG("normalizing key: ~p", [Field]),
    Key = graphql_utils:normalize_key(Field),
    ?DEV_LOG("normalized key: ~p", [Key]),
    {'ok', maps:get(Key, JObj, 'null')}.

