%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_api_cdrs).

-export([summary/2, summary/3
        ,fetch/3
        ]).

-export([interactions/2
        ,legs/3
        ]).

-include("kazoo_proper.hrl").

-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId) ->
    summary(API, AccountId, <<"application/json">>).

-spec summary(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
summary(API, AccountId, Accept) ->
    URL = cdrs_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"accept">>, Accept}]),

    Expectations = [#{'response_codes' => [200]
                     ,'response_headers' => [{"content-type", kz_term:to_list(Accept)}]
                     }
                   ,#{'response_codes' => [204]}
                   ],

    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, CDRId) ->
    URL = cdr_url(AccountId, CDRId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec interactions(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
interactions(API, AccountId) ->
    URL = interactions_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec legs(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
legs(API, AccountId, InteractionId) ->
    URL = legs_url(AccountId, InteractionId),
    RequestHeaders = pqc_cb_api:request_headers(API),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

legs_url(AccountId, InteractionId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "cdrs", "legs", kz_term:to_list(InteractionId)], "/").

interactions_url(AccountId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "cdrs", "interaction"], "/").

cdr_url(AccountId, CDRId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "cdrs", kz_term:to_list(CDRId)], "/").

cdrs_url(AccountId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "cdrs"], "/").
