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
    << <<(normalize_key_char(Char))/binary>> || <<Char>> <= normalize_key_word(Key) >>.

normalize_key_char(C) when is_integer(C), $A =< C, C =< $Z ->
    <<"_", (C + 32)>>;
%% Converts latin capital letters to lowercase, skipping 16#D7 (extended ascii 215) "multiplication sign: x"
normalize_key_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 ->
    <<"_", (C + 32)>>;
normalize_key_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE ->
    %% so we only loop once
    <<"_", (C + 32)>>;
normalize_key_char(C) ->
    <<C>>.

normalize_key_word(Key) ->
    binary:replace(Key, <<"ID">>, <<"_id">>).
