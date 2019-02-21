%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_api_services).

-export([assign/3
        ,available/2
        ]).

-spec assign(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                    pqc_cb_api:response().
assign(API, AccountId, ServicePlanId) ->
    URL = account_service_plan_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    RequestData = kz_json:from_list([{<<"add">>, [ServicePlanId]}]),
    RequestEnvelope = pqc_cb_api:create_envelope(RequestData),

    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:post/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).


-spec available(pqc_cb_api:state(), kz_term:ne_binary()) ->
                       pqc_cb_api:response().
available(API, AccountId) ->
    URL = string:join([account_service_plan_url(AccountId), "available"], "/"),
    RequestHeaders = pqc_cb_api:request_headers(API),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec account_service_plan_url(kz_term:ne_binary()) -> string().
account_service_plan_url(AccountId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "services"], "/").
