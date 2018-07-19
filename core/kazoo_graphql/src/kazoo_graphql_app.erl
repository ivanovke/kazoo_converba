%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_graphql_app).
-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_graphql_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% TODO: graphql should have a mechanism to unload schemas of this name space.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = spawn(fun() -> application:stop(graphql) end),
    'ok'.

