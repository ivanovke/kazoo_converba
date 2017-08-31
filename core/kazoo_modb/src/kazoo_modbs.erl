%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kazoo_modbs).

-export([list_all/0, list_all/1
        ,list_account/1, list_account/2
        ]).

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


-spec list_account(ne_binary()) -> ne_binaries().
-spec list_account(ne_binary(), kz_util:account_format()) -> ne_binaries().
list_account(Account) ->
    list_account(Account, ?REPLICATE_ENCODING).

list_account(Account, Encoding) ->
    AccountId = kz_util:format_account_id(Account, 'unencoded'),
    {'ok', MODBs} = kz_datamgr:db_list([{'startkey', <<AccountId/binary, "-">>}
                                       ,{'endkey', <<AccountId/binary, ".">>}
                                       ]),

    [kz_util:format_account_modb(MODb, Encoding)
     || MODb <- MODBs
    ].
