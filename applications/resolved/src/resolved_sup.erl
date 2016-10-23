%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(resolved_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("resolved.hrl").

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?CACHE(?CACHE_NAME)
                  ,?WORKER('resolved_listener')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(any()) -> sup_init_ret().
init([]) ->
    kz_util:set_startup(),
    UdpServer = {
        resolved_udp_server
        ,{resolved_udp_server, start_link, []}
        ,permanent
        ,2000
        ,worker
        ,[ed_udp_server]
    },
    UdpHandlerSup = {
        resolved_udp_handler_sup
        ,{resolved_udp_handler_sup, start_link, []}
        ,permanent, 2000, supervisor
        ,[resolved_udp_handler_sup]
    },
    ZoneSup = {
        resolved_zone_sup
        ,{resolved_zone_sup, start_link, []}
        ,permanent, 2000, supervisor
        ,[resolved_zone_sup]
    },
    ExtensionSup = {
        resolved_extension_sup
        ,{resolved_extension_sup, start_link, []}
        ,permanent, 2000, supervisor
        ,[resolved_extension_sup]
    },
    Children = [UdpServer, UdpHandlerSup, ZoneSup, ExtensionSup],
    RestartStrategy = {one_for_one, 3600, 4},
    {ok, {RestartStrategy, Children}}.
