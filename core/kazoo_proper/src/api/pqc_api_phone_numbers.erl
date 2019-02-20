%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_api_phone_numbers).

%% Crossbar API test functions
-export([list/3
        ,add/3
        ,activate/3
        ,remove/3
        ]).

-include("kazoo_proper.hrl").

-spec list(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
list(API, AccountId, Number) ->
    URL = number_url(AccountId, Number),
    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([200, 404], fun kz_http:get/2, URL, RequestHeaders).

-spec add(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
add(API, AccountId, Number) ->
    URL = number_url(AccountId, Number),
    RequestHeaders  = pqc_cb_api:request_headers(API),
    RequestEnvelope = pqc_cb_api:create_envelope(kz_json:new()
                                                ,kz_json:from_list([{<<"accept_charges">>, 'true'}])
                                                ),
    pqc_cb_api:make_request([201, 404, 409]
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec remove(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
remove(API, AccountId, Number) ->
    URL = number_url(AccountId, Number),
    RequestHeaders = pqc_cb_api:request_headers(API),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:delete/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec activate(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_term:ne_binary()) ->
                      pqc_cb_api:response().
activate(API, AccountId, Number) ->
    URL = number_url(AccountId, Number, "activate"),
    RequestHeaders  = pqc_cb_api:request_headers(API),
    RequestEnvelope = pqc_cb_api:create_envelope(kz_json:new(), kz_json:from_list([{<<"accept_charges">>, 'true'}])),
    pqc_cb_api:make_request([201, 404, 500]
                           ,fun kz_http:put/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec number_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
number_url(AccountId, Number) ->
    string:join([pqc_api_accounts:account_url(AccountId)
                ,"phone_numbers", kz_term:to_list(kz_http_util:urlencode(Number))
                ]
               ,"/"
               ).

-spec number_url(kz_term:ne_binary(), kz_term:ne_binary(), string()) -> string().
number_url(AccountId, Number, PathToken) ->
    string:join([pqc_api_accounts:account_url(AccountId)
                ,"phone_numbers", kz_term:to_list(kz_http_util:urlencode(Number))
                ,PathToken
                ]
               ,"/"
               ).
