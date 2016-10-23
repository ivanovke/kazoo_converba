%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) root supervisor
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(resolved_zone_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% behaviour callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the supervisor
%% @spec start_link() -> {ok, Pid::pid()}
%%-----------------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%============================================================================
%%% behaviour callbacks
%%%============================================================================
init([]) ->
    ZoneRegistryServer = {resolved_zone_registry_server,
        {resolved_zone_registry_server, start_link, []},
        permanent, 2000, worker, [ed_zone_registry_server]},
    ZoneDataSup = {resolved_zone_data_sup,
        {resolved_zone_data_sup, start_link, []},
        permanent, 2000, supervisor, [resolved_zone_data_sup]},
    Children = [ZoneRegistryServer, ZoneDataSup],
    RestartStrategy = {one_for_one, 3600, 4},
    {ok, {RestartStrategy, Children}}.
