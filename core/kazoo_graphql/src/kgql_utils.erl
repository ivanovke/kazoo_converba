%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(kgql_utils).

-export([normalize_key/1
        ,resolve_jobj_to_list/1, resolve_jobj_to_list/2, resolve_jobj_to_list/3
        ]).

-include("kz_graphql.hrl").

%%------------------------------------------------------------------------------
%% @doc Convert GraphQL camelCase key name to our JSON underscore key name.
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

%% @equiv resolve_jobj_to_list(JObj, 'false')
-spec resolve_jobj_to_list(map()) -> {'ok', map()} | {'error', kz_term:ne_binary()}.
resolve_jobj_to_list(JObj) ->
    resolve_jobj_to_list(JObj, 'false').

%% @equiv resolve_jobj_to_list(JObj, FailOnNull, <<"name">>)
-spec resolve_jobj_to_list(map(), boolean()) -> {'ok', map()} | {'error', kz_term:ne_binary()}.
resolve_jobj_to_list(JObj, FailOnNull) ->
    resolve_jobj_to_list(JObj, FailOnNull, <<"name">>).

%%------------------------------------------------------------------------------
%% @doc Convert JObj map to a list of maps in which each map is value of each
%% original map with original key name is set into a customize key namei inside.
%% @end
%%------------------------------------------------------------------------------
-spec resolve_jobj_to_list(map(), boolean(), kz_term:ne_binary()) -> {'ok', map()} | {'error', kz_term:ne_binary()}.
resolve_jobj_to_list(JObj, _FailOnNull, KeyName) ->
    MapFun = fun(K, V, Acc) -> resolve_jobj_to_list_fold(K, V, Acc, KeyName) end,
    try maps:fold(MapFun, [], JObj) of
        ListOfMaps -> {'ok', ListOfMaps}
    catch
        throw:{'error', {field_null, _Field}} ->
            ?DEV_LOG("field ~s is null", [_Field]),
            {'error', kz_term:to_binary(io_lib:format("field ~s is empty", [_Field]))}
    end.

-spec resolve_jobj_to_list_fold(kz_term:ne_binary(), map(), [map()], kz_term:ne_binary()) -> [map()].
resolve_jobj_to_list_fold(Key, #{}=Value, Acc, KeyName) ->
    [Value#{KeyName => Key} | Acc].

