%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% proplists-like interface to json objects
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_json).

%% don't import the get_keys/1 that fetches keys from the process dictionary
-compile({'no_auto_import', [get_keys/1]}).

-export([to_proplist/1, to_proplist/2]).
-export([to_map/1, to_map/2, from_map/1]).
-export([recursive_to_proplist/1]).

-export([get_first_defined/2, get_first_defined/3]).
-export([get_binary_boolean/2, get_binary_boolean/3]).
-export([get_boolean_value/2, get_boolean_value/3]).
-export([get_integer_value/2, get_integer_value/3]).
-export([get_number_value/2, get_number_value/3]).
-export([get_float_value/2, get_float_value/3]).
-export([get_binary_value/2, get_binary_value/3]).
-export([get_ne_binary_value/2, get_ne_binary_value/3]).
-export([get_lower_binary/2, get_lower_binary/3]).
-export([get_atom_value/2, get_atom_value/3]).
-export([get_string_value/2, get_string_value/3
        ,get_list_value/2, get_list_value/3
        ]).
-export([get_json_value/2, get_json_value/3
        ,get_ne_json_value/2, get_ne_json_value/3
        ]).

-export([is_true/2, is_true/3, is_false/2, is_false/3]).
-export([is_empty/1]).
-export([is_json_object/1, is_json_object/2
        ,are_json_objects/1
        ,is_valid_json_object/1
        ,is_json_term/1
        ]).
-export([is_defined/2]).

-export([filter/2, filter/3
        ,filtermap/2
        ,map/2
        ,foldl/3, foldr/3
        ,find/2, find/3
        ,find_first_defined/2, find_first_defined/3
        ,find_value/3, find_value/4
        ,foreach/2
        ,all/2, any/2
        ,exec/2
        ]).

-export([get_ne_value/2, get_ne_value/3]).
-export([get_value/2, get_value/3
        ,get_values/1, get_values/2
        ,values/1, values/2
        ]).
-export([get_keys/1, get_keys/2]).

-export([set_value/3, set_values/2
        ,insert_value/3, insert_values/2
        ,new/0
        ]).
-export([delete_key/2, delete_key/3
        ,delete_keys/2, prune_keys/2
        ]).
-export([merge_recursive/1
        ,merge_recursive/2
        ,merge_recursive/3
        ,merge/1, merge/2, merge/3
        ,merge_left/2, merge_right/2
        ]).
-export_type([merge_fun/0
             ,merge_arg_2/0
             ,merge_fun_result/0
             ]).

-export([from_list/1, from_list_recursive/1, merge_jobjs/2]).

-export([load_fixture_from_file/2, load_fixture_from_file/3]).

-ifdef(TEST).
-export([fixture/1
        ,fixture/2
        ]).
-endif.

-export([normalize_jobj/1
        ,normalize_jobj/3
        ,normalize/1
        ,normalize_key/1
        ,are_equal/2
        ]).

-export([encode/1, encode/2]).
-export([decode/1, decode/2]).
-export([unsafe_decode/1, unsafe_decode/2]).

-export([flatten/1, flatten/2
        ,expand/1
        ,diff/2
        ]).

-export([sum/2, sum/3]).
-export([sum_jobjs/1, sum_jobjs/2]).
-export_type([sumer/0]).

-export([order_by/3]).

-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-export_type([json_proplist/0
             ,object/0, objects/0
             ,api_object/0, api_objects/0
             ,flat_object/0, flat_objects/0
             ,path/0, paths/0
             ,key/0, keys/0
             ,json_term/0, api_json_term/0, json_terms/0
             ,encode_options/0
             ]).

-spec new() -> object().
-ifdef(JIFFY_NO_MAPS).
new() -> kz_json_tuple:new().
-else.
new() -> kz_json_map:new().
-endif.

-spec encode(json_term()) -> text().
-spec encode(json_term(), encode_options()) -> text().
encode(JObj) -> encode(JObj, []).

encode(JObj, Options) -> jiffy:encode(JObj, Options).

-spec unsafe_decode(iolist() | ne_binary()) -> json_term().
-spec unsafe_decode(iolist() | ne_binary(), ne_binary()) -> json_term().

unsafe_decode(Thing) when is_list(Thing);
                          is_binary(Thing) ->
    unsafe_decode(Thing, <<"application/json">>).

unsafe_decode(JSON, <<"application/json">>) ->
    try jiffy:decode(JSON)
    catch
        'throw':{'error',{_Loc, 'invalid_string'}}=Error ->
            lager:debug("invalid string(near char ~p) in input, checking for unicode", [_Loc]),
            case try_converting(JSON) of
                JSON -> throw({'invalid_json', Error, JSON});
                Converted -> unsafe_decode(Converted)
            end;
        'throw':Error ->
            throw({'invalid_json', Error, JSON});
        _Error:_Reason ->
            throw({'invalid_json', {'error', {0, 'decoder_exit'}}, JSON})
    end.

-spec decode(iolist() | ne_binary()) -> json_term().
-spec decode(iolist() | ne_binary(), ne_binary()) -> json_term().

decode(Thing) when is_list(Thing)
                   orelse is_binary(Thing) ->
    decode(Thing, <<"application/json">>).

decode(JSON, <<"application/json">>) ->
    try unsafe_decode(JSON)
    catch
        _:{'invalid_json', {'error', {_Loc, _Msg}}, _JSON} ->
            lager:debug("decode error ~s near char # ~b", [_Msg, _Loc]),
            log_big_binary(JSON),
            new()
    end.

try_converting(JSON) ->
    case unicode:bom_to_encoding(JSON) of
        {'latin1', 0} ->
            lager:debug("json is latin1, trying as utf8"),
            case unicode:characters_to_binary(JSON, 'latin1', 'utf8') of
                Converted when is_binary(Converted) ->
                    case unicode:bom_to_encoding(Converted) of
                        {'latin1', 0} -> JSON;
                        _ -> Converted
                    end;
                _O ->
                    lager:debug("failed to char_to_bin: ~p", [_O]),
                    JSON
            end;
        _Enc ->
            lager:debug("unknown encoding: ~p", [_Enc]),
            JSON
    end.

-spec log_big_binary(binary()) -> 'ok'.
log_big_binary(<<Bin:500/binary, Rest/binary>>) ->
    lager:debug("bin: ~w", [Bin]),
    log_big_binary(Rest);
log_big_binary(Bin) ->
    lager:debug("bin: ~w", [Bin]).

-spec is_defined(path(), object()) -> boolean().
is_defined(Path, JObj) ->
    'undefined' =/= get_value(Path, JObj).

-spec is_empty(any()) -> boolean().
is_empty(#{}=JObj) -> kz_json_map:is_empty(JObj);
is_empty(JObj) -> kz_json_tuple:is_empty(JObj).

-spec is_json_object(any()) -> boolean().
-spec is_json_object(path(), any()) -> boolean().
is_json_object(#{}=JObj) -> kz_json_map:is_json_object(JObj);
is_json_object(JObj) -> kz_json_tuple:is_json_object(JObj).

is_json_object(Key, JObj) ->
    is_json_object(get_value(Key, JObj)).

-spec are_json_objects(list()) -> boolean().
are_json_objects(JObjs) when is_list(JObjs) ->
    lists:all(fun is_json_object/1, JObjs).

-spec is_valid_json_object(any()) -> boolean().
is_valid_json_object(MaybeJObj) ->
    try
        lists:all(fun(K) ->
                          is_json_term(get_value([K], MaybeJObj))
                  end, ?MODULE:get_keys(MaybeJObj))
    catch
        'throw':_ -> 'false';
        'error':_ -> 'false'
    end.

-spec is_json_term(json_term()) -> boolean().
is_json_term('undefined') -> throw({'error', 'no_atom_undefined_in_jobj_please'});
is_json_term(V) when is_atom(V) -> 'true';
is_json_term(V) when is_binary(V) -> 'true';
is_json_term(V) when is_bitstring(V) -> 'true';
is_json_term(V) when is_integer(V) -> 'true';
is_json_term(V) when is_float(V) -> 'true';
is_json_term(Vs) when is_list(Vs) ->
    lists:all(fun is_json_term/1, Vs);
is_json_term({'json', IOList}) when is_list(IOList) -> 'true';
is_json_term(MaybeJObj) ->
    is_json_object(MaybeJObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Finds out whether 2 JSON objects are recursively identical.
%% @end
%%--------------------------------------------------------------------
-spec are_equal(api_object(), api_object()) -> boolean().
are_equal('undefined', 'undefined') -> 'true';
are_equal('undefined', _) -> 'false';
are_equal(_, 'undefined') -> 'false';
are_equal(JObj1, JObj2) ->
    to_map(JObj1) =:= to_map(JObj2).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Converts top-level proplist to json object, but only if sub-proplists have been converted
%% first.
%% For example:
%% [{a, b}, {c, [{d, e}]}]
%% would be converted to json by
%% kz_json:from_list([{a,b}, {c, kz_json:from_list([{d, e}])}]).
%% the sub-proplist [{d,e}] needs converting before being passed to the next level
%% @end
%%--------------------------------------------------------------------
-spec from_list(json_proplist()) -> object().
-ifdef(JIFFY_NO_MAPS).
from_list(L) when is_list(L) ->
    kz_json_tuple:from_list(L).
-else.
from_list(L) when is_list(L) ->
    kz_json_map:from_list(L).
-endif.

-spec from_list_recursive(json_proplist()) -> object().
from_list_recursive([]) -> new();
from_list_recursive(L)
  when is_list(L) ->
    recursive_from_list(L).

-spec recursive_from_list(list()) -> object().
recursive_from_list([First | _]=List)
  when is_list(List), is_tuple(First) ->
    from_list([{kz_term:to_binary(K), recursive_from_list(V)}
               || {K,V} <- List
              ]);
recursive_from_list(X) when is_float(X) -> X;
recursive_from_list(X) when is_integer(X) -> X;
recursive_from_list(X) when is_atom(X) -> X;
recursive_from_list(X) when is_list(X) ->
    case io_lib:printable_unicode_list(X) of
        'true' -> kz_term:to_binary(X);
        'false' -> [recursive_from_list(Xn) || Xn <- X]
    end;
recursive_from_list(X) when is_binary(X) -> X;
recursive_from_list({_Y, _M, _D}=Date) -> kz_date:to_iso8601_extended(Date);
recursive_from_list({{_, _, _}, {_, _, _}}=DateTime) -> kz_time:iso8601(DateTime);
recursive_from_list(_Else) -> null.

%% Lifted from Jesper's post on the ML (Nov 2016) on merging maps
-spec merge(objects()) -> object().
-spec merge(object(), object()) -> object().
merge([JObjInit | JObjs]) ->
    lists:foldl(fun(JObj, Acc) -> merge(Acc, JObj) end
               ,JObjInit
               ,JObjs
               ).
merge(JObj1, JObj2) ->
    merge(fun merge_right/2, JObj1, JObj2).

-spec merge_left(key(), merge_arg_2()) -> merge_fun_result().
merge_left(_K, {_Dir, 'null'}) -> 'undefined';
merge_left(_K, {_Dir, 'undefined'}) -> 'undefined';
merge_left(_K, {'both', 'null', _Right}) -> 'undefined';
merge_left(_K, {'both', 'undefined', _Right}) -> 'undefined';

merge_left(_K, {'left', Left}) ->
    case is_json_object(Left) of
        'true' ->
            {'ok', merge(fun merge_left/2, Left, new())};
        'false' ->
            {'ok', Left}
    end;
merge_left(_K, {'right', Right}) ->
    case is_json_object(Right) of
        'true' ->
            {'ok', merge(fun merge_left/2, Right, new())};
        'false' ->
            {'ok', Right}
    end;

merge_left(_K, {'both', Left, Right}) ->
    case is_json_object(Left)
        andalso is_json_object(Right)
    of
        'true' ->
            case is_empty(Left) of
                'true' -> {'ok', Left};
                'false' ->
                    {'ok', merge(fun merge_left/2, Left, Right)}
            end;
        'false' ->
            {'ok', Left}
    end.

-spec merge_right(key(), merge_arg_2()) -> merge_fun_result().
merge_right(_K, {_Dir, 'null'}) -> 'undefined';
merge_right(_K, {_Dir, 'undefined'}) -> 'undefined';
merge_right(_K, {'both', _Left, 'null'}) -> 'undefined';
merge_right(_K, {'both', _Left, 'undefined'}) -> 'undefined';

merge_right(_K, {'left', Left}) ->
    case is_json_object(Left) of
        'true' ->
            {'ok', merge(fun merge_right/2, new(), Left)};
        'false' ->
            {'ok', Left}
    end;

merge_right(_K, {'right', Right}) ->
    case is_json_object(Right) of
        'true' ->
            {'ok', merge(fun merge_right/2, new(), Right)};
        'false' ->
            {'ok', Right}
    end;

merge_right(_K, {'both', Left, Right}) ->
    case is_json_object(Left)
        andalso is_json_object(Right)
    of
        'true' ->
            case 0 =:= map_size(Right) of
                'true' -> {'ok', Right};
                'false' -> {'ok', merge(fun merge_right/2, Left, Right)}
            end;
        'false' -> {'ok', Right}
    end.

-type merge_arg_2() :: {'left' | 'right', json_term()} | {'both', json_term(), json_term()}.
-type merge_fun_result() :: 'undefined' | {'ok', json_term()}.
-type merge_fun() :: fun((key(), merge_arg_2()) -> merge_fun_result()).
-spec merge(merge_fun(), object(), object()) -> object().
merge(MergeFun, #{}=Left, #{}=Right) ->
    kz_json_map:merge(MergeFun, Left, Right);
merge(MergeFun, Left, Right) ->
    kz_json_tuple:merge(MergeFun, Left, Right).

%% @public
%% @doc
%% Only a top-level merge.
%% Merges JObj1 into JObj2
%% @end
-spec merge_jobjs(object(), object()) -> object().
merge_jobjs(JObj1, JObj2) ->
    foldr(fun set_value/3, JObj2, JObj1).

-type merge_pred() :: fun((json_term(), json_term()) -> boolean()).

-spec merge_true(any(), any()) -> 'true'.
merge_true(_, _) -> 'true'.

-spec merge_recursive(objects()) -> object().
merge_recursive(JObjs) when is_list(JObjs) ->
    merge_recursive(JObjs, fun merge_true/2).

-spec merge_recursive(objects() | object(), merge_pred() | object()) -> object().
merge_recursive([], Pred) when is_function(Pred, 2) -> new();
merge_recursive([JObj|JObjs], Pred) when is_function(Pred, 2) ->
    lists:foldl(fun(JObj2, JObjAcc) ->
                        merge_recursive(JObjAcc, JObj2, Pred)
                end
               ,JObj
               ,JObjs
               );
merge_recursive(JObj1, JObj2) ->
    merge_recursive(JObj1, JObj2, fun merge_true/2).

-spec merge_recursive(object(), object() | json_term(), merge_pred()) -> object().
merge_recursive(JObj1, JObj2, Pred) when is_function(Pred, 2) ->
    merge_recursive(JObj1, JObj2, Pred, []).

%% inserts values from JObj2 into JObj1
-spec merge_recursive(object(), object() | json_term(), merge_pred(), keys() | []) -> object().
merge_recursive(JObj1, JObj2, Pred, Keys) when is_function(Pred, 2) ->
    case is_json_object(JObj2) of
        'true' -> merge_recursive_fold(JObj1, JObj2, Pred, Keys);
        'false' -> merge_recursive_set(JObj1, JObj2, Pred, Keys)
    end.

-spec merge_recursive_set(object(), json_term(), merge_pred(), keys() | []) -> object().
merge_recursive_set(JObj1, Value, Pred, Keys) ->
    Syek = lists:reverse(Keys),
    Value1 = merge_recursive_get(Syek, JObj1),
    case Pred(Value1, Value) of
        'false' -> JObj1;
        'true' -> set_value(Syek, Value, JObj1)
    end.

-spec merge_recursive_get(keys(), object()) -> json_term() | 'undefined'.
merge_recursive_get(Keys, JObj) ->
    try get_value(Keys, JObj) of
        Value -> Value
    catch
        'error':'badarg' -> 'undefined'
    end.

merge_recursive_fold(JObj1, JObj2, Pred, Keys) ->
    foldl(fun(Key2, Value2, JObj1Acc) ->
                  merge_recursive(JObj1Acc, Value2, Pred, [Key2|Keys])
          end
         ,JObj1
         ,JObj2
         ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sum two (deep) JSON objects.
%% Default sumer function only sums numbers. For other kinds of values,
%% the value from JObj1 is kept untouched. If it is undefined it's the one from JObj2.
%% @end
%%--------------------------------------------------------------------
-spec sum(object(), object()) -> object().
sum(JObj1, JObj2) ->
    sum(JObj1, JObj2, fun default_sumer/2).

-type sumer() :: fun((json_term(), json_term()) -> json_term()).
-spec default_sumer(json_term(), json_term()) -> json_term().
default_sumer(Value1, 'undefined') -> Value1;
default_sumer('undefined', Value2) -> Value2;
default_sumer(Value1, Value2) when is_number(Value1),
                                   is_number(Value2) ->
    Value1 + Value2;
default_sumer(Value1, _) ->
    Value1.

-spec sum(object(), object(), sumer()) -> object().
sum(JObj1, JObj2, Sumer)
  when is_function(Sumer, 2) ->
    sum(JObj1, JObj2, Sumer, []).

-spec sum(object(), object(), sumer(), keys() | []) -> object().
sum(JObj1, JObjOrValue, Sumer, Keys)
  when is_function(Sumer, 2) ->
    case is_json_object(JObjOrValue) of
        'true' ->
            F = fun(Key2, Value2, JObj1Acc) -> sum(JObj1Acc, Value2, Sumer, [Key2|Keys]) end,
            foldl(F, JObj1, JObjOrValue);
        'false' ->
            Syek = lists:reverse(Keys),
            V = get_value(Syek, JObj1),
            set_value(Syek, Sumer(V, JObjOrValue), JObj1)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sum (deep) JSON objects.
%% Default sumer function only sums numbers. For other kinds of values,
%% the value from JObj1 is kept untouched. If it is undefined it takes the value from the other JObjs.
%% @end
%%--------------------------------------------------------------------
-spec sum_jobjs(objects()) -> object().
sum_jobjs(JObjs) -> sum_jobjs(JObjs, fun default_sumer/2).

-spec sum_jobjs(objects(), sumer()) -> object().
sum_jobjs([], Sumer)
  when is_function(Sumer, 2) -> new();
sum_jobjs([JObj], Sumer)
  when is_function(Sumer, 2) ->
    'true' = is_json_object(JObj),
    JObj;
sum_jobjs([FirstJObj|JObjs], Sumer)
  when is_function(Sumer, 2) ->
    lists:foldl(fun default_sumer_fold/2, FirstJObj, JObjs).

-spec default_sumer_fold(object(), json_term()) -> json_term().
default_sumer_fold(JObj, Carry) ->
    sum(Carry, JObj, fun default_sumer/2).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Reorder JSON objects according to the given list of binaries.
%% Given Path MUST resolve to all distinct values in the given objects.
%% These resolved values MUST all be in the list of binaries too.
%% List of binaries MUST NOT contain duplicates.
%% Both lists MUST be of same size.
%% @end
%%--------------------------------------------------------------------
-spec order_by(path(), ne_binaries(), [objects()]) -> objects().
order_by(Path, Ids, ListOfJObjs)
  when is_list(Ids), is_list(ListOfJObjs) ->
    _ = [[put(get_value(Path, JObj), JObj) || JObj <- JObjs]
         || JObjs <- ListOfJObjs
        ],
    [erase(Id) || Id <- Ids].

-spec to_proplist(object() | objects()) ->
                         json_proplist() | json_proplists() | flat_proplist().
-spec to_proplist(path(), object() | objects()) ->
                         json_proplist() | json_proplists() | flat_proplist().
%% Convert a json object to a proplist
%% only top-level conversion is supported
to_proplist(JObjs) when is_list(JObjs) -> [to_proplist(JObj) || JObj <- JObjs];
to_proplist(#{}=JObj) -> kz_json_map:to_proplist(JObj);
to_proplist(JObj) -> kz_json_tuple:to_proplist(JObj).

%% convert everything starting at a specific key
to_proplist(Key, JObj) -> to_proplist(get_json_value(Key, JObj, new())).

-spec recursive_to_proplist(object() | objects()) -> kz_proplist().
recursive_to_proplist(Props) when is_list(Props) ->
    [recursive_to_proplist(V) || V <- Props];
recursive_to_proplist(#{}=JObj) ->
    [{K, recursive_to_proplist(V)} || {K, V} <- kz_json_map:to_proplist(JObj)];
recursive_to_proplist(JObj) ->
    [{K, recursive_to_proplist(V)} || {K, V} <- kz_json_tuple:to_proplist(JObj)].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a json object to a map
%% @end
%%--------------------------------------------------------------------
-spec to_map(object() | objects()) -> map().
-spec to_map(path(), object() | objects()) -> map().
to_map(JObjs) when is_list(JObjs) ->
    lists:foldl(fun to_map_fold/2, #{}, JObjs);
to_map(JObj) ->
    recursive_to_map(JObj).

%% convert everything starting at a specific key
to_map(Key, JObj) ->
    recursive_to_map(get_json_value(Key, JObj, new())).

to_map_fold(JObj, #{}=Map) ->
    maps:merge(Map, recursive_to_map(JObj)).

-spec recursive_to_map(object() | objects() | kz_proplist()) -> map().
recursive_to_map(#{}=JObj) ->
    maps:from_list([{K, recursive_to_map(V)} || {K, V} <- kz_json_map:to_proplist(JObj)]);
recursive_to_map(List) when is_list(List) ->
    [recursive_to_map(Item) || Item <- List];
recursive_to_map(JObj) ->
    maps:from_list([{K, recursive_to_map(V)} || {K, V} <- kz_json_tuple:to_proplist(JObj)]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a map to a json object
%% @end
%%--------------------------------------------------------------------
-spec from_map(map()) -> object().
from_map(Map) when is_map(Map) ->
    recursive_from_map(Map).

-spec recursive_from_map(map()) -> object().
recursive_from_map(Map) when is_map(Map) ->
    from_list([{K, recursive_from_map(V)} || {K, V} <- maps:to_list(Map)]);
recursive_from_map(List) when is_list(List) ->
    [recursive_from_map(Item) || Item <- List];
recursive_from_map(Else) -> Else.

-spec get_json_value(path(), object()) -> api_object().
-spec get_json_value(path(), object(), Default) -> Default | object().
get_json_value(Key, JObj) -> get_json_value(Key, JObj, 'undefined').
get_json_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value ->
            case is_json_object(Value) of
                'true' -> Value;
                'false' -> Default
            end
    end.

-spec get_ne_json_value(path(), object()) -> api_object().
-spec get_ne_json_value(path(), object(), Default) -> Default | object().
get_ne_json_value(Key, JObj) ->
    get_ne_json_value(Key, JObj, 'undefined').
get_ne_json_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value ->
            case is_json_object(Value)
                andalso not is_empty(Value)
            of
                'true' -> Value;
                'false' -> Default
            end
    end.

-type filter_pred() :: fun(({key(), json_term()}) -> boolean()).
-spec filter(filter_pred(), object()) -> object().
-spec filter(filter_pred(), object(), path()) -> object() | objects().

filter(Pred, JObj) when is_function(Pred, 1) ->
    from_list([E || {_,_}=E <- to_proplist(JObj), Pred(E)]).

filter(Pred, JObj, Path) when is_list(Path),
                              is_function(Pred, 1) ->
    Filtered = filter(Pred, get_json_value(Path, JObj)),
    set_value(Path, Filtered, JObj);
filter(Pred, JObj, Key) ->
    filter(Pred, JObj, [Key]).

-type mapper() :: fun((key(), json_term()) -> {key(), json_term()}).
-spec map(mapper(), object() | flat_object()) -> object() | flat_object().
map(F, JObj) when is_function(F, 2) ->
    from_list([F(K, V) || {K,V} <- to_proplist(JObj)]).

-type filtermapper() :: fun((key(), json_term()) -> boolean() | {'true', json_term()}).
-spec filtermap(filtermapper(), object() | flat_object()) -> object() | flat_object().
filtermap(F, JObj) when is_function(F, 2) ->
    from_list(lists:filtermap(fun({K, V}) -> F(K, V) end, to_proplist(JObj))).

-type foreach_fun() :: fun(({key(), json_term()}) -> any()).
-spec foreach(foreach_fun(), object()) -> 'ok'.
foreach(F, JObj) when is_function(F, 1) ->
    lists:foreach(F, to_proplist(JObj)).

-type kv_boolean_pred() :: fun(({key(), json_term()}) -> boolean()).
-spec all(kv_boolean_pred(), object()) -> boolean().
all(Pred, JObj) when is_function(Pred, 1) ->
    lists:all(Pred, to_proplist(JObj)).

-spec any(kv_boolean_pred(), object()) -> boolean().
any(Pred, JObj) when is_function(Pred, 1) ->
    lists:any(Pred, to_proplist(JObj)).

-type folder() :: fun((key(), json_term(), any()) -> any()).
-spec foldl(folder(), any(), object()) -> any().
foldl(F, Acc0, JObj) when is_function(F, 3) ->
    'true' = is_json_object(JObj),
    case is_empty(JObj) of
        'true' -> Acc0;
        'false' ->
            lists:foldl(fun({Key, Value}, Acc1) -> F(Key, Value, Acc1) end
                       ,Acc0
                       ,to_proplist(JObj)
                       )
    end.

-spec foldr(folder(), any(), object()) -> any().
foldr(F, Acc0, JObj) when is_function(F, 3) ->
    'true' = is_json_object(JObj),
    case is_empty(JObj) of
        'true' -> Acc0;
        'false' -> 
            lists:foldr(fun({Key, Value}, Acc1) -> F(Key, Value, Acc1) end
                       ,Acc0
                       ,to_proplist(JObj)
                       )
    end.

-spec get_string_value(path(), object() | objects()) -> api_list().
-spec get_string_value(path(), object(), Default) -> list() | Default.
get_string_value(Key, JObj) ->
    get_string_value(Key, JObj, 'undefined').
get_string_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_list/1)
    end.

-spec get_list_value(path(), object() | objects()) -> api_list().
-spec get_list_value(path(), object() | objects(), Default) -> Default | list().
get_list_value(Key, JObj) ->
    get_list_value(Key, JObj, 'undefined').
get_list_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        List when is_list(List) -> List;
        _Else -> Default
    end.

-spec get_binary_value(path(), object() | objects()) -> api_binary().
-spec get_binary_value(path(), object() | objects(), Default) -> binary() | Default.
get_binary_value(Key, JObj) ->
    get_binary_value(Key, JObj, 'undefined').
get_binary_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_binary/1)
    end.

-spec get_ne_binary_value(path(), object() | objects()) -> api_ne_binary().
-spec get_ne_binary_value(path(), object() | objects(), Default) -> ne_binary() | Default.
get_ne_binary_value(Key, JObj) ->
    get_ne_binary_value(Key, JObj, 'undefined').
get_ne_binary_value(Key, JObj, Default) ->
    case get_binary_value(Key, JObj, Default) of
        Default -> Default;
        <<>> -> Default;
        Value -> Value
    end.

-spec get_lower_binary(path(), object() | objects()) -> api_binary().
-spec get_lower_binary(path(), object() | objects(), Default) -> binary() | Default.
get_lower_binary(Key, JObj) ->
    get_lower_binary(Key, JObj, 'undefined').
get_lower_binary(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_lower_binary/1)
    end.

%% must be an existing atom
-spec get_atom_value(path(), object() | objects()) -> api_atom().
-spec get_atom_value(path(), object() | objects(), Default) -> atom() | Default.
get_atom_value(Key, JObj) ->
    get_atom_value(Key, JObj, 'undefined').
get_atom_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_atom/1)
    end.

-spec get_boolean_value(path(), object() | objects()) -> api_atom().
-spec get_boolean_value(path(), object() | objects(), Default) -> atom() | Default.
get_boolean_value(Key, JObj) ->
    get_boolean_value(Key, JObj, 'undefined').
get_boolean_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_boolean/1)
    end.

-spec get_integer_value(path(), object() | objects()) -> api_integer().
-spec get_integer_value(path(), object() | objects(), Default) -> integer() | Default.
get_integer_value(Key, JObj) ->
    get_integer_value(Key, JObj, 'undefined').
get_integer_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_integer/1)
    end.

-spec get_number_value(path(), object() | objects()) -> api_number().
-spec get_number_value(path(), object() | objects(), Default) -> number() | Default.
get_number_value(Key, JObj) ->
    get_number_value(Key, JObj, 'undefined').
get_number_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_number/1)
    end.

-spec get_float_value(path(), object() | objects()) -> api_float().
-spec get_float_value(path(), object() | objects(), Default) -> float() | Default.
get_float_value(Key, JObj) ->
    get_float_value(Key, JObj, 'undefined').
get_float_value(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:safe_cast(Value, Default, fun kz_term:to_float/1)
    end.

-spec is_false(path(), object() | objects()) -> boolean().
-spec is_false(path(), object() | objects(), Default) -> boolean() | Default.
is_false(Key, JObj) ->
    kz_term:is_false(get_value(Key, JObj)).
is_false(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        V -> kz_term:is_false(V)
    end.

-spec is_true(path(), object() | objects()) -> boolean().
-spec is_true(path(), object() | objects(), Default) -> boolean() | Default.
is_true(Key, JObj) ->
    is_true(Key, JObj, 'false').
is_true(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        V -> kz_term:is_true(V)
    end.

-spec get_binary_boolean(path(), object() | objects()) -> api_ne_binary().
-spec get_binary_boolean(path(), object() | objects(), Default) -> Default | ne_binary().
get_binary_boolean(Key, JObj) ->
    get_binary_boolean(Key, JObj, 'undefined').

get_binary_boolean(Key, JObj, Default) ->
    case get_value(Key, JObj) of
        'undefined' -> Default;
        Value -> kz_term:to_binary(kz_term:is_true(Value))
    end.

-spec get_keys(object() | flat_object()) -> keys() | [keys(),...] | [].
-spec get_keys(path(), object() | flat_object()) -> keys() | [keys(),...] | [].
get_keys(JObj) -> get_keys1(JObj).

get_keys([], JObj) -> get_keys1(JObj);
get_keys(Keys, JObj) -> get_keys1(get_json_value(Keys, JObj, new())).

-spec get_keys1(list() | object() | flat_object()) -> keys() | [keys(),...] | [].
get_keys1(KVs) when is_list(KVs) -> lists:seq(1, length(KVs));
get_keys1(JObj) -> props:get_keys(to_proplist(JObj)).

-spec get_ne_value(path(), object() | objects()) -> api_json_term().
-spec get_ne_value(path(), object() | objects(), Default) -> json_term() | Default.
get_ne_value(Key, JObj) ->
    get_ne_value(Key, JObj, 'undefined').
get_ne_value(Key, JObj, Default) ->
    Value = get_value(Key, JObj),
    case kz_term:is_empty(Value) of
        'true' -> Default;
        'false' -> Value
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find first json object that has a non_empty value for Key.
%% Returns the value at Key
%% @end
%%--------------------------------------------------------------------
-spec find(path(), objects()) -> api_json_term().
-spec find(path(), objects(), Default) -> json_term() | Default.
find(Key, JObjs) ->
    find(Key, JObjs, 'undefined').
find(_, [], Default) -> Default;
find(Key, [JObj|JObjs], Default) when is_list(JObjs) ->
    try get_value(Key, JObj) of
        'undefined' -> find(Key, JObjs, Default);
        V -> V
    catch
        'error':'badarg' -> find(Key, JObjs, Default)
    end.

-spec find_first_defined(paths(), objects()) -> api_json_term().
-spec find_first_defined(paths(), objects(), Default) -> json_term() | Default.
find_first_defined(Keys, JObjs) ->
    find_first_defined(Keys, JObjs, 'undefined').
find_first_defined([], _JObjs, Default) -> Default;
find_first_defined([Key|Keys], JObjs, Default) ->
    try find(Key, JObjs) of
        'undefined' -> find_first_defined(Keys, JObjs, Default);
        V -> V
    catch
        'error':'badarg' -> find(Key, JObjs, Default)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find first json object that has a Value for Key.
%% Returns the json object or 'undefined'
%% @end
%%--------------------------------------------------------------------
-spec find_value(path(), json_term(), objects()) -> api_object().
-spec find_value(path(), json_term(), objects(), Default) -> object() | Default.
find_value(Key, Value, JObjs) ->
    find_value(Key, Value, JObjs, 'undefined').
find_value(_Key, _Value, [], Default) -> Default;
find_value(Key, Value, [JObj|JObjs], Default) ->
    try get_value(Key, JObj) of
        Value -> JObj;
        _Value -> find_value(Key, Value, JObjs, Default)
    catch
        'error':'badarg' -> find_value(Key, Value, JObjs, Default)
    end.

-spec get_first_defined(paths(), object()) -> json_term() | 'undefined'.
-spec get_first_defined(paths(), object(), Default) -> json_term() | Default.
get_first_defined(Keys, JObj) ->
    get_first_defined(Keys, JObj, 'undefined').
get_first_defined([], _JObj, Default) -> Default;
get_first_defined([H|T], JObj, Default) ->
    try get_value(H, JObj) of
        'undefined' -> get_first_defined(T, JObj, Default);
        V -> V
    catch
        'error':'badarg' -> get_first_defined(T, JObj, Default)
    end.

-spec get_value(path(), object() | objects()) -> json_term() | 'undefined'.
-spec get_value(path(), object() | objects(), Default) -> json_term() | Default.
get_value(Key, JObj) ->
    get_value(Key, JObj, 'undefined').

get_value(Key, #{}=JObj, Default) ->
    kz_json_map:get_value(Key, JObj, Default);
get_value(Key, [#{}|_]=JObjs, Default) ->
    kz_json_map:get_value(Key, JObjs, Default);
get_value(Key, JObj, Default) ->
    kz_json_tuple:get_value(Key, JObj, Default).

-spec values(object()) -> json_terms().
-spec values(path(), object()) -> json_terms().
values(JObj) ->
    [get_value(Key, JObj)
     || Key <- ?MODULE:get_keys(JObj)
    ].

values(Key, JObj) ->
    values(get_value(Key, JObj, new())).

%% split the json object into values and the corresponding keys
-spec get_values(object()) -> {json_terms(), keys()} | {[], []}.
get_values(JObj) ->
    lists:foldr(fun(Key, {Vs, Ks}) ->
                        {[get_value(Key, JObj)|Vs], [Key|Ks]}
                end
               ,{[], []}
               ,?MODULE:get_keys(JObj)
               ).

-spec get_values(path(), object()) -> {json_terms(), keys()}.
get_values(Key, JObj) ->
    get_values(get_value(Key, JObj, new())).

%% Figure out how to set the current key among a list of objects

-type set_value_fun() :: {fun((object(), json_term()) -> object()), json_term()} |
                         fun((object()) -> object()).
-type set_value_funs() :: [set_value_fun(),...].

-spec set_values([{path(), json_term()}] | set_value_funs(), object()) -> object().
set_values(KVs, JObj) when is_list(KVs) ->
    lists:foldr(fun set_value_fold/2, JObj, KVs).

-spec set_value_fold(set_value_fun() | {path(), json_term()}, object()) -> object().
set_value_fold({F, V}, JObj) when is_function(F, 2) ->
    F(JObj, V);
set_value_fold(F, JObj) when is_function(F, 1) ->
    F(JObj);
set_value_fold({K, V}, JObj) ->
    set_value(K, V, JObj).

-spec insert_value(path(), json_term(), object()) -> object().
insert_value(Key, Value, JObj) ->
    case get_value(Key, JObj) of
        'undefined' -> set_value(Key, Value, JObj);
        _V -> JObj
    end.

-spec insert_values(json_proplist(), object()) -> object().
insert_values(KVs, JObj) ->
    lists:foldl(fun insert_value_fold/2, JObj, KVs).

-spec insert_value_fold({path(), json_term()}, object()) -> object().
insert_value_fold({Key, Value}, JObj) ->
    insert_value(Key, Value, JObj).

-spec set_value(path(), json_term() | 'null', object() | objects()) -> object() | objects().
set_value(Key, Value, #{}=JObj) ->
    kz_json_map:set_value(Key, Value, JObj);
set_value(Key, Value, [#{}|_]=JObjs) ->
    kz_json_map:set_value(Key, Value, JObjs);
set_value(Key, Value, JObj) ->
    kz_json_tuple:set_value(Key, Value, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete_key(path(), object() | objects()) -> object() | objects().
-spec delete_key(path(), object() | objects(), 'prune' | 'no_prune') -> object() | objects().
delete_key(Keys, JObj) when is_list(Keys) ->
    delete_key(Keys, JObj, 'no_prune');
delete_key(Key, JObj) ->
    delete_key([Key], JObj, 'no_prune').

%%--------------------------------------------------------------------
%% @public
%% @doc
%%  No 'prune' leaves the parent intact (default).
%%  With 'prune': removes the parent key if the result of the delete is an empty list.
%%  So, delete_key([<<"k1">>, <<"k1.1">>], {[{<<"k1">>, {[{<<"k1.1">>, <<"v1.1">>}]}}]}) would result in
%%    'no_prune' -> {[{<<"k1">>, []}]}
%%    'prune' -> {[]}
%% @end
%%--------------------------------------------------------------------
delete_key(Key, JObj, PruneOrNot) when not is_list(Key) ->
    delete_key([Key], JObj, PruneOrNot);
delete_key(Key, #{}=JObj, PruneOrNot) ->
    kz_json_map:delete_key(Key, JObj, PruneOrNot);
delete_key(Key, [#{}|_]=JObj, PruneOrNot) ->
        kz_json_map:delete_key(Key, JObj, PruneOrNot);
delete_key(Key, JObj, PruneOrNot) ->
    kz_json_tuple:delete_key(Key, JObj, PruneOrNot).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete_keys(paths(), object()) -> object().
delete_keys(Keys, JObj) when is_list(Keys) ->
    %% Figure out how to set the current key among a list of objects
    lists:foldr(fun(K, JObj0) -> delete_key(K, JObj0) end, JObj, Keys).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prune_keys(paths(), object()) -> object().
prune_keys(Keys, JObj) when is_list(Keys) ->
    lists:foldr(fun(K, JObj0) -> delete_key(K, JObj0, 'prune') end
               ,JObj
               ,Keys
               ).

%%--------------------------------------------------------------------
%% @doc
%% Read a json fixture file from the filesystem into memory
%% @end
%%--------------------------------------------------------------------
-spec load_fixture_from_file(atom(), nonempty_string() | ne_binary()) ->
                                    object() |
                                    {'error', atom()}.

-spec load_fixture_from_file(atom(), nonempty_string() | ne_binary(), iodata()) ->
                                    object() |
                                    {'error', atom()}.

load_fixture_from_file(App, File) ->
    load_fixture_from_file(App, <<"couchdb">>, File).

load_fixture_from_file(App, Dir, File) ->
    Path = list_to_binary([code:priv_dir(App), "/", kz_term:to_list(Dir), "/", kz_term:to_list(File)]),
    lager:debug("read fixture for kapp ~s from JSON file: ~s", [App, Path]),
    try
        {'ok', Bin} = file:read_file(Path),
        decode(Bin)
    catch
        _Type:{'badmatch',{'error',Reason}} ->
            lager:debug("badmatch error: ~p", [Reason]),
            {'error', Reason};
        _Type:Reason ->
            lager:debug("exception: ~p", [Reason]),
            {'error', Reason}
    end.

-ifdef(TEST).
-spec fixture(file:filename_all()) -> {ok, object()} | {error, not_found}.
fixture(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> {ok, decode(Bin)};
        {error, _} -> {error, not_found}
    end.

-spec fixture(atom(), file:filename_all()) -> {ok, object()} | {error, not_found}.
fixture(App, Path) when is_atom(App) ->
    fixture(filename:join(code:lib_dir(App, test), Path)).
-endif.

%%--------------------------------------------------------------------
%% @doc
%% Normalize a JSON object for storage as a Document
%% All dashes are replaced by underscores, all upper case character are
%% converted to lower case
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_jobj(object()) -> object().
normalize_jobj(JObj) -> normalize(JObj).

-spec normalize(object()) -> object().
normalize(JObj) -> foldl(fun normalize_foldl/3, new(), JObj).

-spec normalize_foldl(key(), json_term(), object()) -> object().
normalize_foldl(_K, 'undefined', JObj) -> JObj;
normalize_foldl(_K, 'null', JObj) -> JObj;
normalize_foldl(K, V, JObj) -> set_value(normalize_key(K), normalize_value(V), JObj).

-spec normalize_value(json_term()) -> json_term().
normalize_value([_|_]=As) -> [normalize_value(A) || A <- As];
normalize_value(Obj) ->
    case is_json_object(Obj) of
        'true' -> normalize(Obj);
        'false' -> Obj
    end.

-spec normalize_key(ne_binary()) -> ne_binary().
normalize_key(Key) when is_binary(Key) ->
    << <<(normalize_key_char(B))>> || <<B>> <= Key>>.

-spec normalize_key_char(char()) -> char().
normalize_key_char($-) -> $_;
normalize_key_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
%% Converts latin capital letters to lowercase, skipping 16#D7 (extended ascii 215) "multiplication sign: x"
normalize_key_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
normalize_key_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
normalize_key_char(C) -> C.

-type search_replace_format() :: {ne_binary(), ne_binary()} |
                                 {ne_binary(), ne_binary(), fun((any()) -> any())}.
-type search_replace_formatters() :: [search_replace_format()].
-spec normalize_jobj(object(), ne_binaries(), search_replace_formatters()) -> object().
normalize_jobj(JObj, RemoveKeys, SearchReplaceFormatters) ->
    'true' = is_json_object(JObj),
    StrippedJObj = lists:foldl(fun search_replace_format/2
                              ,delete_keys(RemoveKeys, JObj)
                              ,SearchReplaceFormatters
                              ),
    normalize_jobj(StrippedJObj).

-spec search_replace_format(search_replace_format(), object()) -> object().
search_replace_format({Old, New}, JObj) ->
    V = get_value(Old, JObj),
    set_value(New, V, delete_key(Old, JObj));
search_replace_format({Old, New, Formatter}, JObj) when is_function(Formatter, 1) ->
    V = get_value(Old, JObj),
    set_value(New, Formatter(V), delete_key(Old, JObj)).

-spec flatten(object() | objects()) -> flat_object() | flat_objects().
flatten(L) when is_list(L) -> [flatten(JObj) || JObj <- L];
flatten(JObj) ->
    from_list(lists:flatten([flatten_key(K,V) || {K,V} <- to_proplist(JObj)])).

-spec flatten(object() | objects(), 'binary_join') -> flat_object() | flat_objects().
flatten(L, 'binary_join') -> keys_to_binary(flatten(L)).

-spec keys_to_binary(object()) -> object().
keys_to_binary(JObj) ->
    ?MODULE:map(fun key_value_to_binary/2, JObj).

-spec key_value_to_binary(key(), json_term()) -> {key(), json_term()}.
key_value_to_binary(K, V) ->
    {key_to_binary(K), V}.

-spec key_to_binary(keys() | key()) -> binary().
key_to_binary(K) when is_list(K) -> kz_binary:join(K, <<"_">>);
key_to_binary(K) -> K.

-spec join_keys(keys() | key(), key()) -> keys().
join_keys(K1, K2) when is_list(K1) -> K1 ++ [K2];
join_keys(K1, K2) -> [K1, K2].

-spec flatten_key(key() | keys(), any()) -> flat_proplist().

flatten_key(Key, Value) ->
    maybe_flatten_object(Key, Value, is_json_object(Value)).

maybe_flatten_object(Key, Value, 'false') when is_list(Key) -> [{Key, Value}];
maybe_flatten_object(Key, Value, 'false') -> [{[Key], Value}];
maybe_flatten_object(Key, Value, 'true') ->
    maybe_flatten_empty(Key, Value, is_empty(Value)).

maybe_flatten_empty(Key, Value, 'true') when is_list(Key) -> [{Key, Value}];
maybe_flatten_empty(Key, Value, 'true') -> [{[Key], Value}];
maybe_flatten_empty(Key, Value, 'false') ->
    [flatten_key(join_keys(Key, K1), V1) || {K1, V1} <- to_proplist(Value)].

-spec expand(flat_object()) -> object().
expand(JObj) ->
    foldl(fun set_value/3, new(), JObj).

-spec diff(object(), object()) -> object().
diff(J1, J2) ->
    L1 = to_proplist(flatten(J1)),
    L2 = to_proplist(flatten(J2)),
    expand(from_list(L1 -- L2)).

-type exec_fun_1() :: fun((object()) -> object()).
-type exec_fun_2() :: {fun((_, object()) -> object()), _}.
-type exec_fun_3() :: {fun((_, _, object()) -> object()), _, _}.
-type exec_fun() :: exec_fun_1() | exec_fun_2() | exec_fun_3().
-type exec_funs() :: [exec_fun(),...].

-spec exec(exec_funs(), object()) -> object().
exec(Funs, JObj) ->
    'true' = is_json_object(JObj),
    lists:foldl(fun exec_fold/2, JObj, Funs).

-spec exec_fold(exec_fun(), object()) -> object().
exec_fold({F, K, V}, C) when is_function(F, 3) -> F(K, V, C);
exec_fold({F, V}, C) when is_function(F, 2) -> F(V, C);
exec_fold(F, C) when is_function(F, 1) -> F(C).
