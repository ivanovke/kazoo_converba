-module(kz_config_accounts).

-export([default_timezone/0
        ,master_account_id/0
        ,master_account_db/0
        ,is_master_account/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(CONFIG_ID, <<"accounts">>).

-spec default_timezone() -> ne_binary().
default_timezone() ->
    kapps_config:get_ne_binary(?CONFIG_ID, <<"default_timezone">>, <<"America/Los_Angeles">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find the system admin from the system_config if set, if not
%% set it to the oldest acccount and return that.
%% @end
%%--------------------------------------------------------------------

-spec master_account_id() -> {'ok', ne_binary()} |
                             {'error', atom()}.
master_account_id() ->
    case kapps_config:get_ne_binary(?CONFIG_ID, <<"master_account_id">>) of
        'undefined' ->
            R = kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_id">>, ['include_docs']),
            find_master_account_id(R);
        Default -> {'ok', Default}
    end.

find_master_account_id({'error', _}=E) -> E;
find_master_account_id({'ok', []}) -> {'error', 'no_accounts'};
find_master_account_id({'ok', Accounts}) ->
    {'ok', OldestAccountId}=Ok =
        kz_docs:oldest([kz_json:get_value(<<"doc">>, Account)
                        || Account <- Accounts
                       ]),
    lager:debug("setting ~s.master_account_id to ~s", [?CONFIG_ID, OldestAccountId]),
    {'ok', _} = kapps_config:set(?CONFIG_ID, <<"master_account_id">>, OldestAccountId),
    Ok.

-spec master_account_db() -> {'ok', ne_binary()} |
                             {'error', any()}.
master_account_db() ->
    case master_account_id() of
        {'error', _}=E -> E;
        {'ok', AccountId} ->
            {'ok', kz_term:format_account_db(AccountId)}
    end.

-spec is_master_account(ne_binary()) -> boolean().
is_master_account(Account) ->
    AccountId = kz_term:format_account_id(Account),
    case master_account_id() of
        {'ok', AccountId} -> 'true';
        _Else -> 'false'
    end.
