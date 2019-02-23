%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_accounts).

%% runner callbacks
-export([api_calls/1
        ,check_response/3
        ,update_model/3
        ]).

-export([create_account/2
        ,delete_account/2
        ,cleanup_accounts/1, cleanup_accounts/2
        ,cleanup/0, cleanup/1

        ,command/2
        ,symbolic_account_id/2
        ]).

-export_type([account_id/0]).

-include("kazoo_proper.hrl").

-type account_id() :: {'call', 'pqc_kazoo_model', 'account_id_by_name', [pqc_cb_api:state() | proper_types:type()]} |
                      kz_term:ne_binary().

-spec command(pqc_kazoo_model:model(), kz_term:ne_binary() | proper_types:type()) -> api_call().
command(Model, Name) ->
    {'call', ?MODULE, 'create_account', [Model, Name]}.

-spec api_calls(pqc_kazoo_model:model()) -> api_calls().
api_calls(Model) ->
    [{1, command(Model, <<?MODULE_STRING>>)}
    ,{1, {'call', ?MODULE, 'delete_account', [Model, <<?MODULE_STRING>>]}}
    ].

-spec symbolic_account_id(pqc_kazoo_model:model(), kz_term:ne_binary() | proper_types:type()) ->
                                 account_id().
symbolic_account_id(Model, Name) ->
    {'call', 'pqc_kazoo_model', 'account_id_by_name', [Model, Name]}.

-spec create_account(pqc_kazoo_model:model(), kz_term:ne_binary()) -> pqc_cb_api:response().
create_account(Model, NewAccountName) ->
    API = pqc_kazoo_model:api(Model),

    case pqc_api_accounts:create(API, NewAccountName) of
        {'error', _, _}=Error -> Error;
        Resp ->
            NewAccountId = pqc_cb_response:account_id(Resp),
            allow_number_additions(NewAccountId),
            Resp
    end.

-spec allow_number_additions(kz_term:ne_binary()) -> {'ok', kzd_accounts:doc()}.
allow_number_additions(AccountId) ->
    {'ok', _Account} = kzd_accounts:update(AccountId
                                          ,[{kzd_accounts:path_allow_number_additions(), 'true'}]
                                          ).

-spec delete_account(pqc_kazoo_model:model(), kz_term:ne_binary()) -> pqc_cb_api:response().
delete_account(Model, Name) ->
    delete_account(Model, Name, pqc_kazoo_model:account_id_by_name(Model, Name)).

delete_account(_Model, _Name, 'undefined') ->
    ?INFO("account ~s not found, not querying SUT", [_Name]),
    {'error', 400, <<>>};
delete_account(Model, Name, {'call', _, _, _}=Call) ->
    AccountId = proper_symb:eval([], Call),
    delete_account(Model, Name, AccountId);
delete_account(Model, _Name, AccountId) ->
    pqc_api_accounts:delete(pqc_kazoo_model:api(Model), AccountId).

-spec cleanup() -> 'ok'.
cleanup() ->
    cleanup_accounts([<<?MODULE_STRING>>]).

-spec cleanup(pqc_cb_api:state()) -> 'ok'.
cleanup(API) ->
    cleanup_accounts(API, [<<?MODULE_STRING>>]).

-spec cleanup_accounts(kz_term:ne_binaries()) -> 'ok'.
cleanup_accounts(AccountNames) ->
    cleanup_accounts(pqc_cb_api:authenticate(), AccountNames).

-spec cleanup_accounts(pqc_cb_api:state(), kz_term:ne_binaries()) -> 'ok'.
cleanup_accounts(API, AccountNames) ->
    _ = [cleanup_account(API, AccountName) || AccountName <- AccountNames],
    kt_cleanup:cleanup_soft_deletes(?KZ_ACCOUNTS_DB).

-spec cleanup_account(pqc_cb_api:state(), kz_term:ne_binary()) -> 'ok'.
cleanup_account(API, AccountName) ->
    _Attempt = try pqc_api_search:search_account_by_name(API, AccountName) of
                   ?FAILED_RESPONSE ->
                       ?ERROR("failed to search for account by name ~s~n", [AccountName]);
                   APIResp ->
                       Data = pqc_cb_response:data(APIResp),
                       case kz_json:get_ne_binary_value([1, <<"id">>], Data) of
                           'undefined' ->
                               check_accounts_db(AccountName);
                           AccountId -> pqc_api_accounts:delete(API, AccountId)
                       end
               catch
                   'throw':{'error', 'socket_closed_remotely'} ->
                       ?ERROR("broke the SUT cleaning up account ~s (~p)~n", [AccountName, API])
               end,
    timer:sleep(1000).% was needed to stop overwhelming the socket, at least locally

check_accounts_db(Name) ->
    AccountName = kzd_accounts:normalize_name(Name),
    ViewOptions = [{'key', AccountName}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_name">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'error', _E} -> ?ERROR("failed to list by name: ~p", [_E]);
        {'ok', JObjs} ->
            ?INFO("deleting from ~s: ~p~n", [?KZ_ACCOUNTS_DB, JObjs]),
            kz_datamgr:del_docs(?KZ_ACCOUNTS_DB, JObjs)
    end.

-spec update_model(pqc_kazoo_model:model(), api_response(), api_call()) -> pqc_kazoo_model:model().
update_model(Model
            ,APIResp
            ,{'call', ?MODULE, 'create_account', [_API, Name]}
            ) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:is_account_missing/2, [Name]}
                           ,{fun pqc_kazoo_model:add_account/3, [Name, APIResp]}
                           ]);
update_model(Model
            ,APIResp
            ,{'call', ?MODULE, 'delete_account', [_API, Name]}
            ) ->
    pqc_util:transition_if(Model
                          ,[{fun pqc_kazoo_model:does_account_exist/2, [Name]}
                           ,{fun pqc_kazoo_model:remove_account/2, [Name]}
                           ]).


-spec check_response(pqc_kazoo_model:model(), api_call(), api_response()) -> boolean().
check_response(Model
              ,{'call', _, 'create_account', [_API, Name]}
              ,APIResult
              ) ->
    check_create_account(Model, Name, APIResult);
check_response(Model
              ,{'call', _, 'delete_account', [_API, Name]}
              ,APIResult
              ) ->
    check_delete_account(pqc_kazoo_model:account_id_by_name(Model, Name), APIResult).

check_delete_account('undefined', {'error', 400, _}) -> 'true';
check_delete_account('undefined', _Result) ->
    ?ERROR("non-400 error when deleting a non-existent account: ~p", [_Result]),
    'false';
check_delete_account(_AccountId, {'error', _Code, _Body}) ->
    ?ERROR("failed to delete account ~s: ~p: ~s", [_AccountId, _Code, _Body]),
    'false';
check_delete_account(_AccountId, _APIResult) -> 'true'.

check_create_account(Model, Name, APIResult) ->
    check_create_account_result(Name, APIResult, pqc_kazoo_model:account_id_by_name(Model, Name)).

check_create_account_result(Name, APIResult, 'undefined') ->
    ?INFO("no account by the name of ~s, should be an account id in ~s"
         ,[Name, APIResult]
         ),
    'undefined' =/= pqc_cb_response:account_id(APIResult);
check_create_account_result(_Name, {'error', 400, _APIResult}, _AccountId) -> 'true';
check_create_account_result(Name, {'error', _Code, _APIResult}, _AccountId) ->
    ?ERROR("account ~s (~s) found, API returned ~p: ~s"
          ,[Name, _AccountId, _Code, _APIResult]
          ),
    'false'.
