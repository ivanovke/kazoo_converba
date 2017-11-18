-module(kz_json_map).

-export([get_value/2, get_value/3
        ,set_value/3
        ,delete_key/3
        ,new/0
        ,from_list/1
        ,to_proplist/1

        ,is_empty/1
        ,is_json_object/1

        ,merge/3
        ]).

-export_type([object/0, objects/0]).

-type object() :: #{kz_json:json_string() => kz_json:json_value()}.
-type objects() :: [object()].

-type api_object() :: object() | 'undefined'.
-type api_objects() :: objects() | 'undefined'.

-spec new() -> object().
new() -> #{}.

-spec get_value(kz_json:path(), object() | objects()) -> kz_json:json_term() | 'undefined'.
-spec get_value(kz_json:path(), object() | objects(), Default) -> kz_json:json_term() | Default.
get_value(Key, JObj) ->
    get_value(Key, JObj, 'undefined').
get_value([Key|Ks], L, Default) when is_list(L) ->
    try
        get_value1(Ks, lists:nth(kz_term:to_integer(Key), L), Default)
    catch
        'error':'badarg' -> Default;
        'error':'badarith' -> Default;
        'error':'function_clause' -> Default
    end;
get_value(K, Doc, Default) ->
    get_value1(K, Doc, Default).

-spec get_value1(kz_json:path(), api_object() | api_objects(), Default) ->
                        kz_json:json_term() | Default.
get_value1([], 'undefined', Default) -> Default;
get_value1([], JObj, _Default) -> JObj;
get_value1(Key, JObj, Default) when not is_list(Key)->
    get_value1([Key], JObj, Default);
get_value1([K|Ks], JObjs, Default) when is_list(JObjs) ->
    try lists:nth(kz_term:to_integer(K), JObjs) of
        'undefined' -> Default;
        JObj1 -> get_value1(Ks, JObj1, Default)
    catch
        _:_ -> Default
    end;
get_value1([K|Ks], #{}=JObj, Default) ->
    get_value1(Ks, maps:get(K, JObj, 'undefined'), Default);
get_value1(_, 'undefined', Default) ->
    Default;
get_value1(_, #{}, Default) ->
    Default;
get_value1(_K, _V, _D) ->
    erlang:error('badarg').

-spec set_value(kz_json:path(), kz_json:json_term() | 'null', object() | objects()) ->
                       object() | objects().
set_value(Keys, Value, JObj) when is_list(Keys) -> set_value1(Keys, Value, JObj);
set_value(Key, Value, JObj) -> set_value1([Key], Value, JObj).

-spec set_value1(kz_json:keys(), kz_json:json_term() | 'null', object() | objects()) -> object() | objects().
set_value1([Key|_]=Keys, Value, []) when not is_integer(Key) ->
    set_value1(Keys, Value, new());
set_value1([Key|T], Value, JObjs) when is_list(JObjs) ->
    Key1 = kz_term:to_integer(Key),
    case Key1 > length(JObjs) of
        %% The object index does not exist so try to add a new one to the list
        'true' ->
            try
                %% Create a new object with the next key as a property
                JObjs ++ [set_value1(T, Value, set_value1([hd(T)], [], new()))]
            catch
                %% There are no more keys in the list, add it unless not an object
                _:_ ->
                    try
                        JObjs ++ [Value]
                    catch _:_ -> erlang:error('badarg')
                    end
            end;
        %% The object index exists so iterate into the object and update it
        'false' ->
            element(1, lists:mapfoldl(fun(E, {Pos, Pos}) ->
                                              {set_value1(T, Value, E), {Pos + 1, Pos}};
                                         (E, {Pos, Idx}) ->
                                              {E, {Pos + 1, Idx}}
                                      end, {1, Key1}, JObjs))
    end;

%% Figure out how to set the current key in an existing object
set_value1([_|_]=Keys, 'null', JObj) -> delete_key(Keys, JObj);
set_value1([_|_]=Keys, 'undefined', JObj) -> delete_key(Keys, JObj);
set_value1([Key1|T], Value, #{}=JObj) ->
    case maps:get(Key1, JObj, 'undefined') of
        #{}=V1 ->
            %% Replace or add a property in an object in the object at this key
            maps:put(Key1, set_value1(T, Value, V1), JObj);
        V1 when is_list(V1) ->
            %% Replace or add a member in an array in the object at this key
            maps:put(Key1, set_value1(T, Value, V1), JObj);
        'undefined' when T == [] ->
            %% This is the final key and doesnt already exist, just add it to this
            %% objects existing properties
            maps:put(Key1, Value, JObj);
        'undefined' ->
            %% This is not the final key and this object does not have this key
            %% so continue looping the keys creating the necessary json as we go
            maps:put(Key1, set_value1(T, Value, new()), JObj);
        _ when T == [] ->
            %% This is the final key and the objects property should just be replaced
            maps:put(Key1, Value, JObj);
        _ ->
            %% This is not the final key and the objects property should just be
            %% replaced so continue looping the keys creating the necessary json as we go
            maps:put(Key1, set_value1(T, Value, new()), JObj)
    end;
%% There are no more keys to iterate through! Override the value here...
set_value1([], Value, _JObj) -> Value.

-spec delete_key(kz_json:path(), object() | objects()) -> object() | objects().
-spec delete_key(kz_json:path(), object() | objects(), 'prune' | 'no_prune') -> object() | objects().
delete_key(Key, JObj) ->
    delete_key(Key, JObj, 'no_prune').
delete_key(Key, JObj, PruneOrNot) when not is_list(Key) ->
    delete_key([Key], JObj, PruneOrNot);
delete_key(Keys, JObj, 'prune') ->
    prune(Keys, JObj);
delete_key(Keys, JObj, 'no_prune') ->
    no_prune(Keys, JObj).

-spec prune(kz_json:keys(), object() | objects()) -> object() | objects().
prune([], JObj) -> JObj;
prune([K], #{}=JObj) ->
    maps:remove(K, JObj);
prune([K|T], #{}=JObj) ->
    case get_value(K, JObj) of
        'undefined' -> JObj;
        #{}=V -> prune_tail(K, T, JObj, V);
        V when is_list(V) -> prune_tail(K, T, JObj, V);
        _ -> erlang:error('badarg')
    end;
prune(_, []) -> [];
prune([K|T], [_|_]=JObjs) ->
    V = lists:nth(kz_term:to_integer(K), JObjs),
    case prune(T, V) of
        #{}=M when 0 =:= map_size(M) -> replace_in_list(K, 'undefined', JObjs, []);
        V -> replace_in_list(K, 'undefined', JObjs, []);
        V1 -> replace_in_list(K, V1, JObjs, [])
    end.

-spec prune_tail(kz_json:key(), kz_json:keys(), object() | objects(), object() | objects()) ->
                        object() | objects().
prune_tail(K, T, #{}=JObj, V) ->
    case prune(T, V) of
        #{}=E when 0 =:= map_size(E) -> maps:remove(K, JObj);
        [] -> maps:remove(K, JObj);
        V1 -> maps:put(K, V1, JObj)
    end.

-spec no_prune(kz_json:keys(), object() | objects()) -> object() | objects().
no_prune([], #{}=JObj) -> JObj;
no_prune([K], #{}=JObj) ->
    maps:remove(K, JObj);
no_prune([K|T], Array) when is_list(Array), is_integer(K) ->
    {Less, [V|More]} = lists:split(K-1, Array),
    case {T, V} of
        {[_|_]=Keys, #{}=JObj} ->
            Less ++ [no_prune(Keys, JObj)] ++ More;
        {[_|_]=Keys, Arr} when is_list(Arr) ->
            Less ++ 'no_prune'(Keys, Arr) ++ More;
        {_,_} -> Less ++ More
    end;
no_prune([K|T], #{}=JObj) ->
    case get_value(K, JObj) of
        'undefined' -> JObj;
        #{}=V ->
            maps:put(K, no_prune(T, V), JObj);
        V when is_list(V) ->
            maps:put(K, no_prune(T, V), JObj);
        _ -> erlang:error('badarg')
    end;
no_prune(_, []) -> [];
no_prune([K|T], [_|_]=JObjs) when is_integer(K) ->
    V = lists:nth(kz_term:to_integer(K), JObjs),
    V1 = no_prune(T, V),
    case V1 =:= V of
        'true' ->
            replace_in_list(K, 'undefined', JObjs, []);
        'false' ->
            replace_in_list(K, V1, JObjs, [])
    end.

replace_in_list(N, _, _, _) when N < 1 ->
    erlang:error('badarg');
replace_in_list(1, 'undefined', [_OldV | Vs], Acc) ->
    lists:reverse(Acc) ++ Vs;
replace_in_list(1, V1, [_OldV | Vs], Acc) ->
    lists:reverse([V1 | Acc]) ++ Vs;
replace_in_list(N, V1, [V | Vs], Acc) ->
    replace_in_list(N-1, V1, Vs, [V | Acc]).

-spec is_empty(any()) -> boolean().
is_empty(#{}=JObj) when 0 =:= map_size(JObj) -> 'true';
is_empty(_) -> 'false'.

-spec is_json_object(any()) -> boolean().
is_json_object(X) -> is_map(X).

-spec from_list(kz_json:json_proplist()) -> object().
from_list(L) ->
    maps:from_list(props:filter_undefined(L)).

-spec to_proplist(object()) -> kz_json:json_proplist().
to_proplist(#{}=JObj) ->
    maps:to_list(JObj).

-spec merge(kz_json:merge_fun(), object(), object()) -> object().
merge(MergeFun, #{}=Left, #{}=Right) ->
    LeftL = lists:sort(maps:to_list(Left)),
    RightL = lists:sort(maps:to_list(Right)),
    merge(MergeFun, LeftL, RightL, []).

merge(_MergeFun, [], [], Acc) ->
    from_list(Acc);
merge(MergeFun, [{KX, VX}|Xs], [], Acc) ->
    merge(MergeFun, Xs, [], f(KX, MergeFun(KX, {'left', VX}), Acc));
merge(MergeFun, [], [{KY, VY}|Ys], Acc) ->
    merge(MergeFun, Ys, [], f(KY, MergeFun(KY, {'right', VY}), Acc));
merge(MergeFun, [{KX, VX}|Xs]=Left, [{KY, VY}|Ys]=Right, Acc) ->
    if
        KX < KY -> merge(MergeFun, Xs, Right, f(KX, MergeFun(KX, {'left', VX}), Acc));
        KX > KY -> merge(MergeFun, Left, Ys, f(KY, MergeFun(KY, {'right', VY}), Acc));
        KX =:= KY -> merge(MergeFun, Xs, Ys, f(KX, MergeFun(KX, {'both', VX, VY}), Acc))
    end.

-spec f(kz_json:key(), kz_json:merge_fun_result(), list()) -> list().
f(_K, 'undefined', Acc) -> Acc;
f(K, {'ok', R}, Acc) -> [{K, R} | Acc].
