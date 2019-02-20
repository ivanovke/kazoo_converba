%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_api_storage).

-export([create/3, create/4]).

-include("kazoo_proper.hrl").

-spec create(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_json:object()) ->
                    pqc_cb_api:response().
create(API, AccountId, StorageDoc) ->
    create(API, AccountId, StorageDoc, 'undefined').

-spec create(pqc_cb_api:state(), kz_term:api_ne_binary(), kz_json:object(), kz_term:api_boolean()) ->
                    pqc_cb_api:response().
create(API, AccountId, StorageDoc, ValidateSettings) ->
    StorageURL = storage_url(AccountId, ValidateSettings),
    RequestHeaders = pqc_cb_api:request_headers(API, [{<<"content-type">>, <<"application/json">>}]),
    pqc_cb_api:make_request([201, 400]
                           ,fun kz_http:put/3
                           ,StorageURL
                           ,RequestHeaders
                           ,kz_json:encode(pqc_cb_api:create_envelope(StorageDoc))
                           ).

storage_url('undefined') ->
    string:join([pqc_cb_api:v2_base_url(), "storage"], "/");
storage_url(AccountId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "storage"], "/").

storage_url(AccountId, 'false') ->
    storage_url(AccountId) ++ "?validate_settings=false";
storage_url(AccountId, _ValidateSettings) ->
    storage_url(AccountId).
