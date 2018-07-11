%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(raptor_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("raptor.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER('raptor_init')
                  ,?WORKER('graphql_init')
                  %% ,?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)
                  ]
       ).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    kz_util:set_startup(),
    {ok, {{one_for_all, 0, 1}, ?CHILDREN}}.

