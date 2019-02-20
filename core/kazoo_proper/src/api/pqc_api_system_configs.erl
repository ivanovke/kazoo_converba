%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_api_system_configs).

%% API
-export([list/1
        ,get/2
        ,get_default/2
        ,get_node/3
        ,set_default/2
        ,patch_default/3
        ,delete/2
        ]).

-include("kazoo_proper.hrl").

-spec list(pqc_cb_api:state()) -> pqc_cb_api:response().
list(API) ->
    URL = configs_url(),
    pqc_cb_api:make_request([200]
                           ,fun kz_http:get/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec get(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
get(API, Id) ->
    URL = config_url(Id),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:get/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec get_default(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
get_default(API, Id) ->
    URL = config_url(Id) ++ "?with_defaults=true",
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:get/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec get_node(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().
get_node(API, Id, NodeId) ->
    URL = config_url(Id, NodeId) ++ "?with_defaults=true",
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:get/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).

-spec set_default(pqc_cb_api:state(), kz_json:object()) -> pqc_cb_api:response().
set_default(API, Config) ->
    URL = config_url(kz_doc:id(Config)),
    Data = pqc_cb_api:create_envelope(Config),

    pqc_cb_api:make_request([200]
                           ,fun kz_http:post/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Data)
                           ).

-spec patch_default(pqc_cb_api:state(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().
patch_default(API, Id, Config) ->
    URL = config_url(Id),
    Data = pqc_cb_api:create_envelope(Config),

    pqc_cb_api:make_request([200]
                           ,fun kz_http:patch/3
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Data)
                           ).

-spec delete(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete(API, Id) ->
    URL = config_url(Id),
    pqc_cb_api:make_request([200, 404]
                           ,fun kz_http:delete/2
                           ,URL
                           ,pqc_cb_api:request_headers(API)
                           ).


-spec configs_url() -> string().
configs_url() ->
    string:join([pqc_cb_api:v2_base_url(), "system_configs"], "/").

-spec config_url(kz_term:ne_binary()) -> string().
config_url(Id) ->
    string:join([pqc_cb_api:v2_base_url(), "system_configs", kz_term:to_list(Id)], "/").

-spec config_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
config_url(Id, NodeId) ->
    string:join([pqc_cb_api:v2_base_url()
                ,"system_configs"
                ,kz_term:to_list(Id)
                ,kz_term:to_list(NodeId)
                ]
               ,"/"
               ).
