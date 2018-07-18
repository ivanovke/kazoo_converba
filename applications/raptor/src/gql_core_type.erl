%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(gql_core_type).

-export([execute/1]).

-include("raptor.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec execute(any()) -> {'ok', any()} | {'error', any()}.
execute(#{'$type' := <<"Account">>}) ->
    ?DEV_LOG("type Account"),
    {'ok', 'Account'};
execute(#{'$type' := <<"account">>}) ->
    ?DEV_LOG("type account"),
    {'ok', 'Account'};
execute(_Other) ->
    ?DEV_LOG("unkown type ~p", [_Other]),
    {'error', 'unknown_type'}.
