%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Sean Wysor
%%%-------------------------------------------------------------------
-module(resolved_maintenance).

-include("resolved.hrl").

-export([flush_zone/1, records/1, register_zone_provider/2, load_dns_records/0]).

register_zone_provider(ZoneName, Handler={_Module, _Function, _Context}) ->
    resolved_zone_data_sup:register_zone_provider({ZoneName, Handler}).

flush_zone(ZoneName) ->
    {ok, Zone} = resolved_zone_registry_server:get(ZoneName),
    resolved_zone_data_server:flush(Zone).


records(Realm) ->
    {ok, AccountDb} = kapps_util:get_account_by_realm(Realm),
    kz_util:format_account_id(AccountDb).

load_dns_records() ->
    register_zone_provider("bot.co.za", {resolved_provider, get_zone, []}),
    register_zone_provider("sean.sean", {resolved_provider, get_zone, []}).




