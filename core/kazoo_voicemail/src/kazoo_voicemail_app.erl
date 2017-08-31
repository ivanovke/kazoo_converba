-module(kazoo_voicemail_app).
-behaviour(application).

-export([start/2
        ,stop/1
        ]).

-spec start(application:start_type(), any()) -> {'ok', pid()}.
start(_Type, _Args) ->
    kazoo_voicemail_sup:start_link().

-spec stop(any()) -> 'ok'.
stop(_State) ->
    'ok'.
