%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_services).

-export([create_service_plan/2
        ,delete_service_plan/2
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-spec create_service_plan(pqc_cb_api:state(), kzd_service_plan:doc()) ->
                                 {'ok', kzd_service_plan:doc()}.
create_service_plan(_API, ServicePlan) ->
    %% No API to add service plans to master account
    %% Doing so manually for now
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    {'ok', OldVsn} = kz_datamgr:save_doc(MasterAccountDb, ServicePlan),
    io:format("old: ~p~n", [OldVsn]),
    Migrate = kazoo_services_maintenance:migrate_service_plan(MasterAccountDb, OldVsn),
    io:format("mig: ~p~n", [Migrate]),
    {'ok', Migrate}.

-spec delete_service_plan(pqc_cb_api:state(), kz_term:ne_binary()) ->
                                 {'ok', kz_json:object()} |
                                 {'error', any()}.
delete_service_plan(_API, ServicePlanId) ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    kz_datamgr:del_doc(MasterAccountDb, ServicePlanId).

-spec cleanup() -> 'ok'.
cleanup() ->
    kazoo_services_maintenance:remove_orphaned_services(),
    kt_cleanup:cleanup_soft_deletes(<<"services">>).
