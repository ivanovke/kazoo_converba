-module(kazoo_voicemail_sup).
-behaviour(supervisor).

-export([start_link/0
        ,init/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(CHILDREN
       ,[?WORKER('kvm_migrate_crawler')]
       ).

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link(?MODULE, []).

-spec init(any()) -> sup_init_ret().
init(_) ->
    Flags = #{'strategy' => 'one_for_one'
             ,'intensity' => 1
             ,'period' => 1
             },
    {'ok', {Flags, ?CHILDREN}}.
