%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_api_ips).

-include("kazoo_proper.hrl").

%% Crossbar API requests
-export([list/1
        ,assign_ips/3
        ,remove/3
        ,fetch/3
        ,assign_ip/3
        ,fetch_hosts/1
        ,fetch_zones/1
        ,fetch_assigned/2
        ,create/2
        ,delete/2
        ]).

-export([ips_url/0, ips_url/1]).

-spec list(pqc_cb_api:state()) -> pqc_cb_api:response().
list(API) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,ips_url()
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec assign_ips(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binaries()) ->
                        pqc_cb_api:response().
assign_ips(API, AccountId, [_|_]=IPs) ->
    Envelope = pqc_cb_api:create_envelope(IPs),

    pqc_cb_api:make_request([200]
                           ,fun kz_http:post/3
                           ,ips_url(AccountId)
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec assign_ip(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                       pqc_cb_api:response().
assign_ip(API, AccountId, IP) ->
    Envelope = pqc_cb_api:create_envelope(kz_json:new()),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:post/3
                           ,ip_url(AccountId, IP)
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec remove(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                    pqc_cb_api:response().
remove(API, AccountId, IP) ->
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:delete/2
                           ,ip_url(AccountId, IP)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                   pqc_cb_api:response().
fetch(API, AccountId, IP) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,ip_url(AccountId, IP)
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch_hosts(pqc_cb_api:state()) -> pqc_cb_api:response().
fetch_hosts(API) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,ip_url("hosts")
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch_zones(pqc_cb_api:state()) -> pqc_cb_api:response().
fetch_zones(API) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,ip_url("zones")
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec fetch_assigned(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
fetch_assigned(API, AccountId) ->
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,ip_url(AccountId, "assigned")
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec create(pqc_cb_api:state(), kz_json:object()) -> pqc_cb_api:response().
create(API, IPObj) ->
    Envelope = pqc_cb_api:create_envelope(IPObj),
    pqc_cb_api:make_request([201, 409]
                           ,fun kz_http:put/3
                           ,ips_url()
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, IP) ->
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:delete/2
                           ,ip_url(IP)
                           ,pqc_cb_api:request_headers(API)
                           ).


-spec ips_url() -> string().
ips_url() ->
    string:join([pqc_cb_api:v2_base_url(), "ips"], "/").

-spec ips_url(pqc_cb_accounts:account_id()) -> string().
ips_url(AccountId) ->
    string:join([pqc_api_accounts:account_url(AccountId), "ips"], "/").

-spec ip_url(kz_term:text()) -> string().
ip_url(IP) ->
    string:join([pqc_cb_api:v2_base_url(), "ips", kz_term:to_list(IP)], "/").

-spec ip_url(pqc_cb_accounts:account_id(), kz_term:text()) -> string().
ip_url(AccountId, IP) ->
    string:join([pqc_api_accounts:account_url(AccountId), "ips", kz_term:to_list(IP)], "/").
