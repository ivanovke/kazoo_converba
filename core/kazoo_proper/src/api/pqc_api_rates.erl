%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_api_rates).

-export([delete/2, delete/3
        ,fetch/2, fetch/3
        ,summary/1, summary/2
        ,rate/2, rate/3
        ,rate_account/3
        ]).

-include("kazoo_proper.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-spec delete(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, RateId) ->
    delete(API, RateId, ?KZ_RATES_DB).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, RateId, RatedeckId) ->
    URL = rate_url(RateId, RatedeckId),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:delete/2
                           ,URL ++ "&should_soft_delete=false"
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, RateId) ->
    fetch(API, RateId, ?KZ_RATES_DB).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, RateId, RatedeckId) ->
    URL = rate_url(RateId, RatedeckId),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:get/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec summary(pqc_cb_api:state()) -> pqc_cb_api:response().
summary(API) ->
    summary(API, ?KZ_RATES_DB).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, RatedeckId) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,rates_url() ++ "?ratedeck_id=" ++ kz_term:to_list(RatedeckId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec rate(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
rate(API, DID) ->
    rate(API, DID, ?KZ_RATES_DB).

-spec rate(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
rate(API, DID, RatedeckId) ->
    URL = rate_number_url(RatedeckId, DID),
    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([200, 500]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec rate_account(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
rate_account(API, AccountId, DID) ->
    URL = string:join([pqc_api_accounts:account_url(AccountId), "rates", "number", kz_term:to_list(DID)], "/"),
    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([200, 500]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

rates_url() ->
    string:join([pqc_cb_api:v2_base_url(), "rates"], "/").

rate_number_url(RatedeckId, DID) ->
    rate_did_url(pqc_cb_api:v2_base_url(), DID) ++ "?ratedeck_id=" ++ kz_term:to_list(RatedeckId).

rate_url(ID, RatedeckId) ->
    string:join([pqc_cb_api:v2_base_url(), "rates", kz_term:to_list(ID)], "/")
        ++ "?ratedeck_id=" ++ kz_term:to_list(RatedeckId).

rate_did_url(Base, DID) ->
    string:join([Base, "rates", "number", kz_term:to_list(kz_http_util:urlencode(DID))], "/").
