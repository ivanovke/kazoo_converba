-module(kz_docs).

-export([oldest/1]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a list of accounts this returns the id of the oldest
%% @end
%%--------------------------------------------------------------------
-spec oldest(kz_json:objects()) ->
                    {'ok', ne_binary()} |
                    {'error', 'no_docs'}.
oldest([]) -> {'error', 'no_docs'};
oldest([First|Docs]) ->
    {_, OldestDocID} =
        lists:foldl(fun(Doc, {Created, _}=Eldest) ->
                            Older = kz_doc:created(Doc),
                            case Older < Created  of
                                'true' -> {Older, kz_doc:id(Doc)};
                                'false' -> Eldest
                            end
                    end
                   ,{kz_doc:created(First), kz_doc:id(First)}
                   ,Docs),
    {'ok', OldestDocID}.
