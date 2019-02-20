%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_api_ledgers).

%% API functions
-export([fetch/2, fetch/3
        ,fetch_by_source/3, fetch_by_source/4
        ,credit/3, debit/3
        ]).

-include("kazoo_proper.hrl").


%% API functions
-spec fetch(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, ?NE_BINARY=AccountId) ->
    fetch(API, AccountId, <<"application/json">>).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, ?NE_BINARY=AccountId, ?NE_BINARY=AcceptType) ->
    LedgersURL = ledgers_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"accept">>, AcceptType}]),

    Expectations = #{'response_codes' => [200]
                    ,'response_headers' => [{<<"content-type">>, AcceptType}]
                    },

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,LedgersURL
                           ,RequestHeaders
                           ).

-spec fetch_by_source(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                             pqc_cb_api:response().
fetch_by_source(API, ?NE_BINARY=AccountId, ?NE_BINARY=SourceType) ->
    fetch_by_source(API, AccountId, SourceType, <<"application/json">>).

-spec fetch_by_source(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                             pqc_cb_api:response().
fetch_by_source(API, ?NE_BINARY=AccountId, SourceType, ?NE_BINARY=AcceptType) ->
    LedgersURL = ledgers_source_url(AccountId, SourceType),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"accept">>, AcceptType}]),

    Expectations = [#{'response_codes' => [200]
                     ,'response_headers' => [{<<"content-type">>, AcceptType}]
                     }
                   ,#{'response_codes' => [204]}
                   ],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,LedgersURL
                           ,RequestHeaders
                           ).

-spec credit(pqc_cb_api:state(), kz_term:ne_binary(), kzd_ledger:doc()) ->
                    pqc_cb_api:response().
credit(API, ?NE_BINARY=AccountId, Ledger) ->
    LedgersURL = ledgers_credit_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = #{'response_codes' => [201]},

    Envelope = pqc_cb_api:create_envelope(Ledger),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,LedgersURL
                           ,RequestHeaders
                           ,kz_json:encode(Envelope)
                           ).

-spec debit(pqc_cb_api:state(), kz_term:ne_binary(), kzd_ledger:doc()) ->
                   pqc_cb_api:response().
debit(API, ?NE_BINARY=AccountId, Ledger) ->
    LedgersURL = ledgers_debit_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    Expectations = #{'response_codes' => [201]},

    Envelope = pqc_cb_api:create_envelope(Ledger),

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:put/3
                           ,LedgersURL
                           ,RequestHeaders
                           ,kz_json:encode(Envelope)
                           ).

-spec ledgers_url(kz_term:ne_binary()) -> string().
ledgers_url(AccountId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "ledgers"], "/").

-spec ledgers_source_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
ledgers_source_url(AccountId, SourceType) ->
    string:join([pqc_api_accounts:account_url(AccountId), "ledgers", kz_term:to_list(SourceType)], "/").

-spec ledgers_credit_url(kz_term:ne_binary()) -> string().
ledgers_credit_url(AccountId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "ledgers", "credit"], "/").

-spec ledgers_debit_url(kz_term:ne_binary()) -> string().
ledgers_debit_url(AccountId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "ledgers", "debit"], "/").
