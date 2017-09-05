%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_account_test).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.

-include_lib("eunit/include/eunit.hrl").

%% For format_account_* tests
-export([format_account_id_raw/1
        ,format_account_id_encoded/1
        ,format_account_id_unencoded/1
        ,format_account_mod_id_from_year_month/1
        ,format_account_mod_id_from_now/1
        ,format_account_modb_raw/1
        ,format_account_modb_encoded/1
        ,format_account_modb_unencoded/1
        ]).

-define(AN_ACCOUNT_ID, <<"4fe69c5b61015084f1fe5684abc6e502">>).

-define(ID, <<"_id">>).
-define(TREE, <<"pvt_tree">>).

-define(MASTER_ACCOUNT_ID, <<"account0000000000000000000000001">>).
-define(SUB_ACCOUNT_ID, <<"account0000000000000000000000002">>).
-define(SUB_SUB_ACCOUNT_ID, <<"account0000000000000000000000003">>).

validate_fixtures_test_() ->
    {'ok', Schema} = kz_json_schema:fload(<<"accounts">>),
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kzd_account:fetch(?SUB_ACCOUNT_ID),
    {'ok', SubSubAccount} = kzd_account:fetch(?SUB_SUB_ACCOUNT_ID),
    [{"validate master account fixture", ?_assertMatch({'ok', _}, validate(Schema, MasterAccount))}
    ,{"validate sub account fixture", ?_assertMatch({'ok', _}, validate(Schema, SubAccount))}
    ,{"validate sub-sub account fixture", ?_assertMatch({'ok', _}, validate(Schema, SubSubAccount))}
    ].

fetch_test_() ->
    [?_assertEqual({error,invalid_db_name}, kzd_account:fetch(undefined))
    ,?_assertEqual(undefined, kzd_account:fetch_realm(undefined))
    ,?_assertEqual(undefined, kzd_account:fetch_name(undefined))
    ].

new_test_() ->
    Account = kzd_account:new(),
    [{"validate new returns a JSON object", ?_assert(kz_json:is_json_object(Account))}
    ,{"validate new sets the correct doc type", ?_assertEqual(<<"account">>, kz_json:get_value(<<"pvt_type">>, Account))}
    ].

type_test_() ->
    [{"validate type returns the expected value", ?_assertEqual(<<"account">>, kzd_account:type())}].

id_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    [{"validate id returns the expected value", ?_assertEqual(?MASTER_ACCOUNT_ID, kzd_account:id(MasterAccount))}].

name_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"name">>, MasterAccount),
    Updated = kzd_account:set_name(MasterAccount, <<"updated">>),
    [{"validate fetch_name returns the expected value", ?_assertEqual(<<"Master Account">>, kzd_account:fetch_name(?MASTER_ACCOUNT_ID))}
    ,{"validate name returns the expected value", ?_assertEqual(<<"Master Account">>, kzd_account:name(MasterAccount))}
    ,{"validate name returns 'undefined' if not found", ?_assertEqual('undefined', kzd_account:name(Missing))}
    ,{"validate name can return a default value if not found", ?_assertEqual(<<"default">>, kzd_account:name(Missing, <<"default">>))}
    ,{"validate set_name changes the name", ?_assertEqual(<<"updated">>, kzd_account:name(Updated))}
    ].

realm_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"realm">>, MasterAccount),
    Updated = kzd_account:set_realm(MasterAccount, <<"updated">>),
    [{"validate fetch_realm returns the expected value", ?_assertEqual(<<"4a6863.sip.2600hz.local">>, kzd_account:fetch_realm(?MASTER_ACCOUNT_ID))}
    ,{"validate realm returns the expected value", ?_assertEqual(<<"4a6863.sip.2600hz.local">>, kzd_account:realm(MasterAccount))}
    ,{"validate realm returns 'undefined' if not found", ?_assertEqual('undefined', kzd_account:realm(Missing))}
    ,{"validate realm can return a default value if not found", ?_assertEqual(<<"default">>, kzd_account:realm(Missing, <<"default">>))}
    ,{"validate set_realm changes the realm", ?_assertEqual(<<"updated">>, kzd_account:realm(Updated))}
    ].

language_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"language">>, MasterAccount),
    Updated = kzd_account:set_language(MasterAccount, <<"updated">>),
    [{"validate language returns the expected value", ?_assertEqual(<<"en-US">>, kzd_account:language(MasterAccount))}
    ,{"validate language returns 'undefined' if not found", ?_assertEqual('undefined', kzd_account:language(Missing))}
    ,{"validate language can return a default value if not found", ?_assertEqual(<<"default">>, kzd_account:language(Missing, <<"default">>))}
    ,{"validate set_language changes the language", ?_assertEqual(<<"updated">>, kzd_account:language(Updated))}
    ].

timezone_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"timezone">>, MasterAccount),
    Invalid = kz_json:set_value(<<"timezone">>, <<"inherit">>, MasterAccount),
    Updated = kzd_account:set_timezone(MasterAccount, <<"updated">>),
    Default = kz_config_accounts:default_timezone(),
    [{"validate timezone returns the expected value", ?_assertEqual(<<"America/Los_Angeles">>, kzd_account:timezone(MasterAccount))}
    ,{"validate timezone returns the default if not found", ?_assertEqual(Default, kzd_account:timezone(Missing))}
    ,{"validate timezone returns the default if set to 'inherit'", ?_assertEqual(Default, kzd_account:timezone(Invalid))}
    ,{"validate timezone can return a default value if not found", ?_assertEqual(<<"default">>, kzd_account:timezone(Missing, <<"default">>))}
    ,{"validate set_timezone changes the timezone", ?_assertEqual(<<"updated">>, kzd_account:timezone(Updated))}
    ].

parent_account_id_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kzd_account:fetch(?SUB_ACCOUNT_ID),
    {'ok', SubSubAccount} = kzd_account:fetch(?SUB_SUB_ACCOUNT_ID),
    [{"verify that fetching the parent id of the master account returns 'undefined'"
     ,?_assertEqual('undefined', kzd_account:parent_account_id(MasterAccount))
     }
    ,{"verify that fetching the parent id of sub account is the master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, kzd_account:parent_account_id(SubAccount))
     }
    ,{"verify fetching the parent id of a sub-sub account is the direct ancestor"
     ,?_assertEqual(?SUB_ACCOUNT_ID, kzd_account:parent_account_id(SubSubAccount))
     }
    ].

tree_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    {'ok', SubAccount} = kzd_account:fetch(?SUB_ACCOUNT_ID),
    {'ok', SubSubAccount} = kzd_account:fetch(?SUB_SUB_ACCOUNT_ID),
    [?_assertEqual([], kzd_account:tree(MasterAccount))
    ,?_assertEqual([?MASTER_ACCOUNT_ID], kzd_account:tree(SubAccount))
    ,?_assertEqual([?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID], kzd_account:tree(SubSubAccount))
    ].

notification_preference_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_notification_preference">>, MasterAccount),
    Updated = kzd_account:set_notification_preference(MasterAccount, <<"notify">>),
    [{"validate notification_preference returns the expected value", ?_assertEqual(<<"teletype">>, kzd_account:notification_preference(MasterAccount))}
    ,{"validate notification_preference returns 'undefined' if not found", ?_assertEqual('undefined', kzd_account:notification_preference(Missing))}
    ,{"validate set_notification_preference changes the notification_preference", ?_assertEqual(<<"notify">>, kzd_account:notification_preference(Updated))}
    ].

enabled_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    Disabled = kzd_account:disable(MasterAccount),
    ReEnabled = kzd_account:enable(MasterAccount),
    [{"validate is_enabled returns the expected value", ?_assert(kzd_account:is_enabled(MasterAccount))}
    ,{"validate disable returns the expected value", ?_assertNot(kzd_account:is_enabled(Disabled))}
    ,{"validate enable returns the expected value", ?_assert(kzd_account:is_enabled(ReEnabled))}
    ].

api_key_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_api_key">>, MasterAccount),
    Updated = kzd_account:set_api_key(MasterAccount, <<"updated">>),
    [{"validate api_key returns the expected value", ?_assertEqual(<<"apikey0000000000000000000000000000000000000000000000000000000001">>, kzd_account:api_key(MasterAccount))}
    ,{"validate api_key returns 'undefined' if not found", ?_assertEqual('undefined', kzd_account:api_key(Missing))}
    ,{"validate set_api_key changes the api_key", ?_assertEqual(<<"updated">>, kzd_account:api_key(Updated))}
    ].

superduper_admin_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_superduper_admin">>, MasterAccount),
    Updated = kzd_account:set_superduper_admin(MasterAccount, 'false'),
    [{"validate superduper_admin returns the expected value", ?_assert(kzd_account:is_superduper_admin(MasterAccount))}
    ,{"validate superduper_admin returns 'false' if not found", ?_assertNot(kzd_account:is_superduper_admin(Missing))}
    ,{"validate set_superduper_admin changes the superduper_admin", ?_assertNot(kzd_account:is_superduper_admin(Updated))}
    ].

allow_number_additions_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    Missing = kz_json:delete_key(<<"pvt_wnm_allow_additions">>, MasterAccount),
    Updated = kzd_account:set_allow_number_additions(MasterAccount, 'false'),
    [{"validate allow_number_additions returns the expected value", ?_assert(kzd_account:allow_number_additions(MasterAccount))}
    ,{"validate allow_number_additions returns 'undefined' if not found", ?_assertNot(kzd_account:allow_number_additions(Missing))}
    ,{"validate set_allow_number_additions changes the allow_number_additions", ?_assertNot(kzd_account:allow_number_additions(Updated))}
    ].

trial_time_test_() ->
    Now = kz_time:now_s(),
    Passed = kzd_account:set_trial_expiration(kzd_account:new(), Now - 10000),
    Active = kzd_account:set_trial_expiration(kzd_account:new(), Now + 10000),

    [{"testing expired trial accounts are computed as such"
     ,?_assertEqual('true', kzd_account:trial_has_expired(Passed, Now))
     }
    ,{"testing current trial accounts are computed as such"
     ,?_assertEqual('false', kzd_account:trial_has_expired(Active, Now))
     }
    ,{"testing that current trial accounts have proper time left computed"
     ,?_assertEqual(10000, kzd_account:trial_time_left(Active, Now))
     }
    ,{"testing that expired trial accounts have proper time since expiration computed"
     ,?_assertEqual(-10000, kzd_account:trial_time_left(Passed, Now))
     }
    ].

reseller_test_() ->
    {'ok', MasterAccount} = kzd_account:fetch(?MASTER_ACCOUNT_ID),
    DemotedMasterAccount = kzd_account:demote(MasterAccount),
    Missing = kz_json:delete_key(<<"pvt_reseller">>, MasterAccount),
    {'ok', SubAccount} = kzd_account:fetch(?SUB_ACCOUNT_ID),
    Demoted = kzd_account:demote(SubAccount),
    RePromoted = kzd_account:promote(SubAccount),
    [{"validate master account is a reseller if improperly configured", ?_assert(kzd_account:is_reseller(DemotedMasterAccount))}
    ,{"validate master account is a reseller if the value is missing", ?_assert(kzd_account:is_reseller(Missing))}
    ,{"validate is_reseller returns the expected value", ?_assert(kzd_account:is_reseller(SubAccount))}
    ,{"validate promote returns the expected value", ?_assert(kzd_account:is_reseller(RePromoted))}
    ,{"validate demote returns the expected value", ?_assertNot(kzd_account:is_reseller(Demoted))}
    ].

validate(Schema, Device) ->
    kz_json_schema:validate(Schema
                           ,Device
                           ,[{'schema_loader_fun', fun kz_json_schema:fload/1}]
                           ).

normalize_account_name_test_() ->
    [?_assertEqual(undefined, kzd_account:normalize_name(undefined))
    ,?_assertEqual(<<"blip2blop">>, kzd_account:normalize_name(<<"Blip#2!Blop">>))
    ].

is_in_account_hierarchy_test_() ->
    [?_assertEqual(false, kzd_account:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID))
    ,?_assertEqual(false, kzd_account:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID))
    ,?_assertEqual(false, kzd_account:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID, true))
    ,?_assertEqual(false, kzd_account:is_in_account_hierarchy(undefined, ?AN_ACCOUNT_ID, false))
    ,?_assertEqual(false, kzd_account:is_in_account_hierarchy(?AN_ACCOUNT_ID, undefined, false))
    ,?_assertEqual(false, kzd_account:is_in_account_hierarchy(?AN_ACCOUNT_ID, undefined, true))
    ,?_assertEqual(true, kzd_account:is_in_account_hierarchy(?AN_ACCOUNT_ID, ?AN_ACCOUNT_ID, true))
    ].

is_system_admin_test_() ->
    [?_assertEqual(false, kzd_account:is_system_admin(undefined))
    ].

is_account_enabled_test_() ->
    [?_assertEqual(false, kzd_account:is_account_enabled(undefined))
    ].

is_account_expired_test_() ->
    [?_assertEqual(false, kzd_account:is_account_expired(undefined))
    ].

account_formats_test_() ->
    AccountId = <<A:2/binary, B:2/binary, Rest:28/binary>> = kz_binary:rand_hex(16),
    AccountDbUn = list_to_binary(["account/", A, "/", B, "/", Rest]),
    AccountDbEn = list_to_binary(["account%2F", A, "%2F", B, "%2F", Rest]),

    {Y, M, _} = erlang:date(),
    TS = kz_time:current_tstamp(),
    Now = os:timestamp(),
    Year = kz_term:to_binary(Y),
    Month = kz_time:pad_month(M),

    MODbId = list_to_binary([AccountId, "-", Year, Month]),
    MODbEn = list_to_binary([AccountDbEn, "-", Year, Month]),
    MODbUn = list_to_binary([AccountDbUn, "-", Year, Month]),

    Formats = [AccountId, AccountDbUn, AccountDbEn
              ,MODbId, MODbEn, MODbUn
              ],
    %% Note: the whole point of exporting some of these is so that function_clause can be caught
    Funs = [{fun kzd_account:format_account_id/1, AccountId}
           ,{fun ?MODULE:format_account_id_raw/1, AccountId}
           ,{fun ?MODULE:format_account_id_encoded/1, AccountDbEn}
           ,{fun ?MODULE:format_account_id_unencoded/1, AccountDbUn}
           ,{fun kzd_account:format_account_db/1, AccountDbEn}
           ,{fun kzd_account:format_account_mod_id/1, MODbEn}
           ,{fun ?MODULE:format_account_mod_id_from_year_month/1, MODbEn}
           ,{fun ?MODULE:format_account_mod_id_from_now/1, MODbEn}
           ,{fun kzd_account:format_account_modb/1, MODbId}
           ,{fun ?MODULE:format_account_modb_raw/1, MODbId}
           ,{fun ?MODULE:format_account_modb_encoded/1, MODbEn}
           ,{fun ?MODULE:format_account_modb_unencoded/1, MODbUn}
           ],
    [{format_title(Fun, Format, Expected)
     ,format_assert(Fun, Format, Expected)
     }
     || {Fun, Expected} <- Funs,
        Format <- Formats
    ] ++
        [?_assertEqual('undefined', kzd_account:format_account_id('undefined', 'raw'))
        ,?_assertEqual(<<"accounts">>, kzd_account:format_account_id(<<"accounts">>, 'raw'))
        ,?_assertEqual(MODbEn, kzd_account:format_account_id(AccountDbEn, TS))
        ,?_assertEqual(MODbEn, kzd_account:format_account_mod_id(AccountDbEn, TS))
        ,?_assertEqual(undefined, kzd_account:format_account_id('undefined', Year, Month))
        ,?_assertEqual(MODbEn, kzd_account:format_account_id(AccountDbEn, Year, Month))
        ,?_assertEqual(MODbEn, kzd_account:format_account_id(AccountDbEn, Year, M))
        ,?_assertEqual(?KZ_TASKS_DB, kzd_account:format_account_id(?KZ_TASKS_DB, 'raw'))
        ,?_assertEqual(<<"bla">>, kzd_account:format_account_id(<<"bla">>, 'raw'))
        ].

format_assert(Fun, Format, Expected) ->
    Matchable = format_title(Fun, Format, Expected),
    case {is_simple_modb_converter(Matchable), Format} of
        {'true', ?MATCH_ACCOUNT_RAW(_)} -> ?_assertException('error', 'function_clause', Fun(Format));
        {'true', ?MATCH_ACCOUNT_ENCODED(_)} -> ?_assertException('error', 'function_clause', Fun(Format));
        {'true', ?MATCH_ACCOUNT_UNENCODED(_)} -> ?_assertException('error', 'function_clause', Fun(Format));
        {_Else, Format} -> ?_assertEqual(Expected, Fun(Format))
    end.

format_title(Fun, Format, Expected) ->
    lists:flatten(
      io_lib:format("~p converting ~s to ~s", [Fun, Format, Expected])
     ).

is_simple_modb_converter("#Fun<kzd_account.format_account_modb.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kzd_account_test.format_account_modb_raw.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kzd_account_test.format_account_modb_encoded.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kzd_account_test.format_account_modb_unencoded.1>"++_) -> 'true';
is_simple_modb_converter(_Else) -> 'false'.

format_account_id_raw(F) -> kzd_account:format_account_id(F, 'raw').
format_account_id_encoded(F) -> kzd_account:format_account_id(F, 'encoded').
format_account_id_unencoded(F) -> kzd_account:format_account_id(F, 'unencoded').
format_account_mod_id_from_year_month(F) ->
    {Year, Month, _} = erlang:date(),
    kzd_account:format_account_mod_id(F, Year, Month).
format_account_mod_id_from_now(F) ->
    kzd_account:format_account_mod_id(F, os:timestamp()).
format_account_modb_raw(F) -> kzd_account:format_account_modb(F, 'raw').
format_account_modb_encoded(F) -> kzd_account:format_account_modb(F, 'encoded').
format_account_modb_unencoded(F) -> kzd_account:format_account_modb(F, 'unencoded').
