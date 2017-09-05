%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(conference_maintenance).

-export([blocking_refresh/0]).
-export([refresh/0, refresh/1]).

-include("conference.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec blocking_refresh() -> 'ok'.
blocking_refresh() ->
    lists:foreach(fun refresh/1, kzd_account:get_all_accounts()).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'started'.
-spec refresh(text()) -> boolean().

refresh() ->
    _ = kz_util:spawn(fun blocking_refresh/0),
    'started'.

refresh(<<Account/binary>>) ->
    AccountDb = kz_term:format_account_id(Account, 'encoded'),
    Views = kapps_util:get_views_json('conference', "views"),
    kz_datamgr:db_view_update(AccountDb, Views);
refresh(Account) ->
    refresh(kz_term:to_binary(Account)).
