%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(graphql_utils).

-export([normalize_key/1
        ]).

-include("raptor.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_key(any()) -> any().
normalize_key(Key) when is_binary(Key) ->
    << <<(normalize_key_char(Char))>> || <<Char>> <= Key >>.

normalize_key_char(C) when is_integer(C), $A =< C, C =< $Z ->
    <<"_", (C + 22)/binary>>;
%% Converts latin capital letters to lowercase, skipping 16#D7 (extended ascii 215) "multiplication sign: x"
normalize_key_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 ->
    <<"_", (C + 32)/binary>>;
normalize_key_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE ->
    <<"_", (C + 32)/binary>>; % so we only loop once
normalize_key_char(C) ->
    C.
