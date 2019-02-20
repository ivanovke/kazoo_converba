%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_api_recordings).

-export([list/2
        ,fetch/3
        ,fetch_binary/3, fetch_tunneled/3
        ,delete/3
        ]).

-include("kazoo_proper.hrl").

-spec list(pqc_cb_api:state(), pqc_cb_accounts:account_id()) -> pqc_cb_api:response().
list(API, AccountId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:get/2
                           ,recordings_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch(API, AccountId, RecordingId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200]}
                           ,fun kz_http:get/2
                           ,recordings_url(AccountId, RecordingId)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch_binary(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_binary(API, AccountId, RecordingId) ->
    pqc_cb_api:make_request([#{'response_codes' => [200]
                              ,'response_headers' => [{"content-type", "audio/mpeg"}]
                              }
                            ]
                           ,fun kz_http:get/2
                           ,recordings_url(AccountId, RecordingId)
                           ,pqc_cb_api:request_headers(API, [{<<"accept">>, "audio/mpeg"}])
                           ).

-spec fetch_tunneled(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_tunneled(API, AccountId, RecordingId) ->
    pqc_cb_api:make_request([#{'response_codes' => [200]
                              ,'response_headers' => [{"content-type", "audio/mpeg"}]
                              }
                            ]
                           ,fun kz_http:get/2
                           ,recordings_url(AccountId, RecordingId) ++ "?accept=audio/mpeg"
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec delete(pqc_cb_api:state(), pqc_cb_accounts:account_id(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, AccountId, RecordingId) ->
    pqc_cb_api:make_request(#{'response_codes' => [200, 404]}
                           ,fun kz_http:delete/2
                           ,recordings_url(AccountId, RecordingId)
                           ,pqc_cb_api:request_headers(API)
                           ).

recordings_url(AccountId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "recordings"], "/").

recordings_url(AccountId, RecordingId) ->
    string:join([recordings_url(AccountId), kz_term:to_list(RecordingId)], "/").
