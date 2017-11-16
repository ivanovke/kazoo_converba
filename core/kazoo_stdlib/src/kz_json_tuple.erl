-module(kz_json_tuple).

-export([get_value/2, get_value/3
        ,set_value/3
        ,delete_key/3
        ,new/0
        ]).

-export_type([object/0, objects/0]).

%% How do we wrap proplists to denote they're json objects?
%% -define(JSON_WRAPPER(Proplist), {struct, Proplist}).
%% -define(IS_JSON_GUARD(Obj), is_tuple(Obj)
%%         andalso element(1, Obj) =:= 'struct'
%%         andalso is_list(element(2, Obj))
%%        ).
-define(JSON_WRAPPER(Proplist), {Proplist}).

-define(EMPTY_JSON_OBJECT, ?JSON_WRAPPER([])).

-type object() :: ?JSON_WRAPPER(kz_json:json_proplist()) | ?EMPTY_JSON_OBJECT.
-type objects() :: [object()].

-type api_object() :: object() | 'undefined'.
-type api_objects() :: objects() | 'undefined'.

-spec new() -> object().
new() -> ?EMPTY_JSON_OBJECT.

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
get_value1([K|Ks], ?JSON_WRAPPER(Props), Default) ->
    get_value1(Ks, props:get_value(K, Props), Default);
get_value1(_, undefined, Default) ->
    Default;
get_value1(_, ?JSON_WRAPPER(_), Default) ->
    Default;
get_value1(_K, _V, _D) ->
    erlang:error(badarg).

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
set_value1([Key1|T], Value, ?JSON_WRAPPER(Props)) ->
    case lists:keyfind(Key1, 1, Props) of
        {Key1, ?JSON_WRAPPER(_)=V1} ->
            %% Replace or add a property in an object in the object at this key
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, set_value1(T, Value, V1)}));
        {Key1, V1} when is_list(V1) ->
            %% Replace or add a member in an array in the object at this key
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, set_value1(T, Value, V1)}));
        {Key1, _} when T == [] ->
            %% This is the final key and the objects property should just be replaced
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, Value}));
        {Key1, _} ->
            %% This is not the final key and the objects property should just be
            %% replaced so continue looping the keys creating the necessary json as we go
            ?JSON_WRAPPER(lists:keyreplace(Key1, 1, Props, {Key1, set_value1(T, Value, new())}));
        'false' when T == [] ->
            %% This is the final key and doesnt already exist, just add it to this
            %% objects existing properties
            ?JSON_WRAPPER(Props ++ [{Key1, Value}]);
        'false' ->
            %% This is not the final key and this object does not have this key
            %% so continue looping the keys creating the necessary json as we go
            ?JSON_WRAPPER(Props ++ [{Key1, set_value1(T, Value, new())}])
    end;

%% There are no more keys to iterate through! Override the value here...
set_value1([], Value, _JObj) -> Value.

-spec delete_key(kz_json:path(), object() | objects()) -> object() | objects().
-spec delete_key(kz_json:path(), object() | objects(), 'prune' | 'no_prune') -> object() | objects().
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
delete_key(Key, JObj, 'prune') when not is_list(Key) ->
    prune([Key], JObj);
delete_key(Key, JObj, 'no_prune') when not is_list(Key) ->
    no_prune([Key], JObj);
delete_key(Keys, JObj, 'prune') ->
    prune(Keys, JObj);
delete_key(Keys, JObj, 'no_prune') ->
    no_prune(Keys, JObj).

-spec prune(kz_json:keys(), object() | objects()) -> object() | objects().
prune([], JObj) -> JObj;
prune([K], ?JSON_WRAPPER(JProps)) ->
    case lists:keydelete(K, 1, JProps) of
        [] -> new();
        L -> ?JSON_WRAPPER(L)
    end;
prune([K|T], ?JSON_WRAPPER(_)=JObj) ->
    case get_value(K, JObj) of
        'undefined' -> JObj;
        ?JSON_WRAPPER(_)=V -> prune_tail(K, T, JObj, V);
        V when is_list(V) -> prune_tail(K, T, JObj, V);
        _ -> erlang:error('badarg')
    end;
prune(_, []) -> [];
prune([K|T], [_|_]=JObjs) ->
    V = lists:nth(kz_term:to_integer(K), JObjs),
    case prune(T, V) of
        ?EMPTY_JSON_OBJECT -> replace_in_list(K, 'undefined', JObjs, []);
        V -> replace_in_list(K, 'undefined', JObjs, []);
        V1 -> replace_in_list(K, V1, JObjs, [])
    end.

-spec prune_tail(kz_json:key(), kz_json:keys(), object() | objects(), object() | objects()) ->
                        object() | objects().
prune_tail(K, T, ?JSON_WRAPPER(Props), V) ->
    case prune(T, V) of
        ?EMPTY_JSON_OBJECT -> ?JSON_WRAPPER(lists:keydelete(K, 1, Props));
        [] -> ?JSON_WRAPPER(lists:keydelete(K, 1, Props));
        V1 -> ?JSON_WRAPPER([{K, V1} | lists:keydelete(K, 1, Props)])
    end.

-spec no_prune(kz_json:keys(), object() | objects()) -> object() | objects().
no_prune([], ?JSON_WRAPPER(_)=JObj) -> JObj;
no_prune([K], ?JSON_WRAPPER(Props)) ->
    case lists:keydelete(K, 1, Props) of
        [] -> new();
        L -> ?JSON_WRAPPER(L)
    end;
no_prune([K|T], Array) when is_list(Array), is_integer(K) ->
    {Less, [V|More]} = lists:split(K-1, Array),
    case {T, V} of
        {[_|_]=Keys, ?JSON_WRAPPER(_)=JObj} ->
            Less ++ [no_prune(Keys, JObj)] ++ More;
        {[_|_]=Keys, Arr} when is_list(Arr) ->
            Less ++ 'no_prune'(Keys, Arr) ++ More;
        {_,_} -> Less ++ More
    end;
no_prune([K|T], ?JSON_WRAPPER(Props)=JObj) ->
    case get_value(K, JObj) of
        'undefined' -> JObj;
        ?JSON_WRAPPER(_)=V ->
            ?JSON_WRAPPER([{K, no_prune(T, V)} | lists:keydelete(K, 1, Props)]);
        V when is_list(V) ->
            ?JSON_WRAPPER([{K, no_prune(T, V)} | lists:keydelete(K, 1, Props)]);
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
