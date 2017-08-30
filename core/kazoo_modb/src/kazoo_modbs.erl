%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kazoo_modbs).

-export([list_all/0, list_all/1]).

-include("kazoo_modb.hrl").

-define(REPLICATE_ENCODING, 'encoded').

-spec list_all() -> ne_binaries().
-spec list_all(kz_util:account_format()) -> ne_binaries().
list_all() ->
    list_all(?REPLICATE_ENCODING).

list_all(Encoding) ->
    {'ok', Databases} = kz_datamgr:db_list([{'startkey', <<"account/">>}
                                           ,{'endkey', <<"account0">>}
                                           ]
                                          ),
    [kz_util:format_account_modb(Db, Encoding)
     || Db <- Databases,
        'modb' =:= kz_datamgr:db_classification(Db)
    ].
