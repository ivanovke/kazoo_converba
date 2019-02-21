-module(pqc_api_accounts).

-export([create/2
        ,delete/2
        ]).

-export([account_url/1]).

-include("kazoo_proper.hrl").

-spec create(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
create(API, NewAccountName) ->
    RequestData = kz_json:from_list([{<<"name">>, NewAccountName}]),
    RequestEnvelope = pqc_cb_api:create_envelope(RequestData),

    pqc_cb_api:make_request([201]
                           ,fun kz_http:put/3
                           ,account_url(pqc_cb_api:auth_account_id(API))
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId) ->
    URL = account_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    pqc_cb_api:make_request([200], fun kz_http:delete/2, URL, RequestHeaders).

-spec account_url(kz_term:ne_binary() | map()) -> string().
account_url(#{}=API) ->
    account_url(pqc_cb_api:auth_account_id(API));
account_url(?NE_BINARY=AccountId) ->
    string:join([pqc_cb_api:v2_base_url(), "accounts", kz_term:to_list(AccountId)], "/").
