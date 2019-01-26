%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Trigger jobs for execution
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_tasks_trigger).
-behaviour(gen_server).

-export([start_link/0]).
-export([status/0]).
%% Handlers for automatic compaction SUP commands
-export([auto_compaction/1, auto_compaction/2]).
-export([browse_dbs_for_triggers/1]).

%%% gen_server callbacks
-export([init/1
        ,handle_cast/2
        ,handle_call/3
        ,handle_info/2
        ,code_change/3
        ,terminate/2
        ]).

-ifdef(TEST).
-export([sort_by_disk_size/1
        ]).
-endif.

-include("tasks.hrl").
-include_lib("couchbeam/include/couchbeam.hrl"). %% Contains #server{} record definition

-define(RECORD_TO_PROPLIST(RecName, Record),
        lists:zip([kz_term:to_binary(K) || K <- record_info('fields', RecName)],
                  tl(tuple_to_list(Record)))).
-define(SERVER, {'via', 'kz_globals', ?MODULE}).

-record(state, {minute_ref = minute_timer() :: reference()
               ,hour_ref = hour_timer() :: reference()
               ,day_ref = day_timer() :: reference()
               ,browse_dbs_ref = browse_dbs_timer() :: reference() %%TODO: gen_listen for DB news!
               ,auto_compaction_pid = 'undefined' :: pid() | 'undefined'
               }).
-record('auto_compaction', {'call_id' :: kz_term:ne_binary()
                           ,'total_dbs_to_compact' :: pos_integer()
                           ,'total_dbs_already_compacted' = 0 :: non_neg_integer()
                           ,'current_db' = 'undefined' :: kz_term:ne_binary() | 'undefined'
                           ,'recovered_bytes' = 0 :: non_neg_integer()
                           ,'running' = 'false' :: boolean()
                           ,'started' = kz_time:now_s() :: kz_time:gregorian_seconds()
                           ,'finished' = 'undefined' :: kz_time:gregorian_seconds() | 'undefined'
                           }).
-type state() :: #state{}.
-type auto_compaction() :: #auto_compaction{}.

-define(AUTO_COMPACTION_VIEW, <<"auto_compaction/crossbar_listing">>).
-define(CLEANUP_TIMER
       ,kapps_config:get_pos_integer(?CONFIG_CAT, <<"browse_dbs_interval_s">>, ?SECONDS_IN_DAY)).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec status() -> kz_term:proplist().
status() ->
    gen_server:call(?SERVER, 'status').

%%------------------------------------------------------------------------------
%% @doc Handler for automatic compaction SUP command
%% @end
%%------------------------------------------------------------------------------
-spec auto_compaction(kz_term:ne_binary()) -> kz_term:proplist() | 'ok'.
auto_compaction(<<"status">>) ->
    gen_server:call(?SERVER, 'auto_compaction_status');
auto_compaction(<<"history">>) ->
    auto_compaction_history(5);
auto_compaction(Else) ->
    io:format("Invalid argument: ~p~n", [Else]).

%%------------------------------------------------------------------------------
%% @doc Handler for automatic compaction SUP command
%% @end
%%------------------------------------------------------------------------------
-spec auto_compaction(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
auto_compaction(<<"history">>, BinN) when is_binary(BinN) ->
    auto_compaction(<<"history">>, kz_term:to_integer(BinN));
auto_compaction(<<"history">>, N) when is_integer(N)
                                       andalso N < 1 ->
    io:format("Invalid argument \"~p\"~n", [N]);
auto_compaction(<<"history">>, N) when is_integer(N) ->
    auto_compaction_history(N).


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    case gen_server:start_link(?SERVER, ?MODULE, [], []) of
        {'error', {'already_started', Pid}} ->
            'true' = link(Pid),
            {'ok', Pid};
        Other -> Other
    end.


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    _ = process_flag('trap_exit', 'true'),
    kz_util:put_callid(?MODULE),
    lager:debug("started ~s", [?MODULE]),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('status', _From, #state{minute_ref = Minute
                                   ,hour_ref = Hour
                                   ,day_ref = Day
                                   ,browse_dbs_ref = Browse
                                   }=State) ->
    Timers = [{'minute', erlang:read_timer(Minute)}
             ,{'hour', erlang:read_timer(Hour)}
             ,{'day', erlang:read_timer(Day)}
             ,{'cleanup', erlang:read_timer(Browse)}
             ],
    {'reply', Timers, State};

handle_call('auto_compaction_status', _From, State) ->
    {'reply', auto_compaction_status(State#state.auto_compaction_pid), State};

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call ~p from ~p", [_Request, _From]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'cleanup_finished', Ref}, #state{browse_dbs_ref = Ref}=State) ->
    lager:debug("cleanup finished for ~p, starting timer", [Ref]),
    {'noreply', State#state{browse_dbs_ref = browse_dbs_timer()}, 'hibernate'};

handle_cast({'set_auto_compaction_pid', Pid}, State) ->
    {'noreply', State#state{'auto_compaction_pid' = Pid}};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'EXIT', _Pid, normal}, State) ->
    lager:debug("job ~p terminated normally", [_Pid]),
    {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    lager:error("job ~p crashed: ~p", [_Pid, _Reason]),
    {noreply, State};

handle_info({timeout, Ref, _Msg}, #state{minute_ref = Ref}=State) ->
    spawn_jobs(Ref, ?TRIGGER_MINUTELY),
    {'noreply', State#state{minute_ref = minute_timer()}};

handle_info({timeout, Ref, _Msg}, #state{hour_ref = Ref}=State) ->
    spawn_jobs(Ref, ?TRIGGER_HOURLY),
    {'noreply', State#state{hour_ref = hour_timer()}};

handle_info({timeout, Ref, _Msg}, #state{day_ref = Ref}=State) ->
    spawn_jobs(Ref, ?TRIGGER_DAILY),
    {'noreply', State#state{day_ref = day_timer()}};

handle_info({timeout, Ref, _Msg}, #state{browse_dbs_ref = Ref}=State) ->
    _Pid = kz_util:spawn(fun browse_dbs_for_triggers/1, [Ref]),
    lager:debug("cleaning up in ~p(~p)", [_Pid, Ref]),
    {'noreply', State};

handle_info(_Info, State) ->
    lager:debug("unhandled message ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec minute_timer() -> reference().
minute_timer() ->
    erlang:start_timer(?MILLISECONDS_IN_MINUTE, self(), ok).

-spec hour_timer() -> reference().
hour_timer() ->
    erlang:start_timer(?MILLISECONDS_IN_HOUR, self(), ok).

-spec day_timer() -> reference().
day_timer() ->
    erlang:start_timer(?MILLISECONDS_IN_DAY, self(), ok).

-spec browse_dbs_timer() -> reference().
browse_dbs_timer() ->
    Expiry = ?CLEANUP_TIMER,
    lager:debug("starting cleanup timer for ~b s", [Expiry]),
    erlang:start_timer(Expiry * ?MILLISECONDS_IN_SECOND, self(), ok).


-spec spawn_jobs(reference(), kz_term:ne_binary()) -> ok.
spawn_jobs(Ref, Binding) ->
    CallId = make_callid(Ref, Binding),
    _Pid = erlang:spawn_link(fun () ->
                                     _ = kz_util:put_callid(CallId),
                                     tasks_bindings:map(Binding, [])
                             end),
    lager:debug("binding ~s triggered ~p via ~p", [Binding, _Pid, Ref]).

-spec make_callid(reference(), kz_term:ne_binary()) -> kz_term:ne_binary().
make_callid(Ref, Binding) ->
    Key = lists:last(binary:split(Binding, <<$.>>, [global])),
    Id = ref_to_id(Ref),
    <<"task_", Key/binary, "_", Id/binary>>.

ref_to_id(Ref) ->
    Bin = list_to_binary(io_lib:format("~p", [Ref])),
    Start = <<"#Ref<">>,
    StartSize = byte_size(Start),
    Size = byte_size(Bin) - StartSize - 1,
    <<Start:StartSize/binary, Id:Size/binary, ">">> = Bin,
    Id.

%% =======================================================================================
%% Start - Automatic Compaction Section
%% =======================================================================================

%%------------------------------------------------------------------------------
%% @doc Print automatic compaction history according to user's request.
%% @end
%%------------------------------------------------------------------------------
-spec auto_compaction_history(pos_integer()) -> 'ok'.
auto_compaction_history(N) ->
    {'ok', AccountId} = kapps_util:get_master_account_id(),
    QSJObj = kz_json:from_list([{<<"page_size">>, N}]),
    Setters = [{fun cb_context:set_query_string/2, QSJObj}
              ,{fun cb_context:set_account_id/2, AccountId}
              ,{fun cb_context:set_should_paginate/2, 'true'}
              ,{fun cb_context:set_api_version/2, <<"v2">>} %% pagination
              ],
    Context = cb_context:setters(cb_context:new(), Setters),
    Context1 =  crossbar_view:load_modb(Context, ?AUTO_COMPACTION_VIEW, ['include_docs']),
    case cb_context:resp_data(Context1) of
        [] ->
            io:format("Not history found yet");
        [_|_] = JObjs ->
            Header = ["call_id"
                     ,"to_compact"
                     ,"compacted"
                     ,"recovered_space"
                     ,"started_at"
                     ,"finished_at"
                     ,"exec_time"
                     ],
            %% Last 8 is because the count of | (pipe) chars within `FStr' string.
            HLine = lists:flatten(lists:duplicate(21+10+9+15+19+19+10+8, "-")),
            %% Format string for printing header and values of the table including "columns".
            FStr = "|~.21s|~.10s|~.9s|~.15s|~.19s|~.19s|~.10s|~n",
            %% Print top line of table, then prints the header and then another line below.
            io:format("~s~n" ++ FStr ++ "~s~n", [HLine] ++ Header ++ [HLine]),
            lists:foreach(fun(Obj) -> print_auto_compaction_result(Obj, FStr) end, JObjs),
            io:format("~s~n", [HLine])
    end.

-spec print_auto_compaction_result(kz_json:object(), string()) -> string().
print_auto_compaction_result(JObj, FStr) ->
    Doc = kz_json:get_json_value(<<"doc">>, JObj),
    Str = fun(K, From) -> kz_json:get_string_value(K, From) end,
    Int = fun(K, From) -> kz_json:get_integer_value(K, From) end,
    StartInt = Int(<<"started">>, Doc),
    EndInt = Int(<<"finished">>, Doc),
    Row = [Str(<<"call_id">>, Doc)
          ,Str(<<"total_dbs_to_compact">>, Doc)
          ,Str(<<"total_dbs_already_compacted">>, Doc)
          ,kz_util:pretty_print_bytes(Int(<<"recovered_bytes">>, Doc))
          ,kz_term:to_list(kz_time:pretty_print_datetime(StartInt))
          ,kz_term:to_list(kz_time:pretty_print_datetime(EndInt))
          ,kz_term:to_list(kz_time:pretty_print_elapsed_s(EndInt - StartInt))
          ],
    io:format(FStr, Row).

%%------------------------------------------------------------------------------
%% @doc Set automatic compaction's PID, that way it knows which process to ask about
%% current status of the automatic compaction job.
%% @end
%%------------------------------------------------------------------------------
-spec set_auto_compaction_pid(pid()) -> 'ok'.
set_auto_compaction_pid(Pid) ->
    gen_server:cast(?SERVER, {'set_auto_compaction_pid', Pid}).

-spec auto_compaction_status(pid()) -> tuple().
auto_compaction_status('undefined') ->
    <<"not running">>;
auto_compaction_status(Pid) ->
    Pid ! {'compaction_status', self()},
    receive
        {Pid, Status} ->
            Status
    after 5000 ->
        lager:debug("timeout retrieving automatic compaction status")
    end.

%%------------------------------------------------------------------------------
%% @doc Entry point for starting the automatic compaction job.
%%
%% This functions gets triggered by the `browse_dbs_ref' based on `browse_dbs_timer'
%% function. By default it triggers the action 1 day after the timer starts.
%%
%% This function spawns a process (`cleanup/0') in charge of triggering dbs compaction.
%% After starting the process it gets the process id (pid) and stores it within gen_server's
%% state, that way when the user asks for current status of the compaction job via SUP
%% command the gen_server knows who to ask about the status. Then it waits for the spawned
%% process to finish in order to mark the cleanup process as completed.
%% @end
%%------------------------------------------------------------------------------
-spec browse_dbs_for_triggers(atom() | reference()) -> 'ok'.
browse_dbs_for_triggers(Ref) ->
    lager:debug("starting cleanup pass of databases"),
    {Pid, CompactRef} = kz_util:spawn_monitor(fun cleanup/0, []),
    'ok' = set_auto_compaction_pid(Pid),
    %% Wait for cleanup/0 spawned process to finish.
    receive
        {'DOWN', CompactRef, 'process', Pid, _Info} ->
            'ok'
    end,
    'ok' = set_auto_compaction_pid('undefined'),
    lager:debug("pass completed for ~p", [Ref]),
    gen_server:cast(?SERVER, {'cleanup_finished', Ref}).

%%------------------------------------------------------------------------------
%% @doc Retrieves all the dbs within the current system, get their disk and data size
%% and then sort dbs by disk_size (bigger dbs have bigger priority). Then spawn a process
%% (cleanup/3) that will trigger db compaction one db at the time and start listening for
%% all the events from this process to keep the current compaction job status information
%% so it has up to date information to return when it is asked for the current status.
%%
%% cleanup/0 process also listens for `auto_compaction_status' request and returns its
%% current state.
%%
%% After cleanup/3 finishes its execution this function receives the collected stats after
%% running the compaction job and saves it within BigCouch under the master_account's modb
%% so it can be used later to report automatic compaction history via SUP command.
%% @end
%%------------------------------------------------------------------------------
-spec cleanup() -> 'ok'.
cleanup() ->
    CallId = <<"cleanup_pass_", (kz_binary:rand_hex(4))/binary>>,
    kz_util:put_callid(CallId),
    #{'server' := {_App, #server{}=Conn}} = kzs_plan:plan(),
    Sorted = sort_by_disk_size(get_dbs_disk_and_data(Conn)),
    State = #auto_compaction{'call_id' = CallId
                            ,'total_dbs_to_compact' = length(Sorted)
                            ,'running' = 'true'
                            },
    NewState = loop(kz_util:spawn_monitor(fun cleanup/3, [self(), Sorted, Conn]), State),
    save_compaction_job_info(NewState).

-spec cleanup(pid()
             ,[{kz_term:ne_binary(), kt_compactor_worker:db_disk_and_data()}]
             ,#server{}
             ) -> 'ok'.
cleanup(From, Sorted, Conn) ->
    lists:foreach(fun(Db) -> do_cleanup(From, Db, Conn) end, Sorted).

-spec do_cleanup(pid()
                ,{kz_term:ne_binary(), kt_compactor_worker:db_disk_and_data()}
                ,#server{}
                ) -> 'ok'.
do_cleanup(From, {Db, {OldDiskSize, _OldDataSize}}, Conn) ->
    From ! {'current_db', Db},
    cleanup_pass(Db),
    case kt_compactor_worker:get_db_disk_and_data(Conn, Db) of
      {NewDiskSize, _} ->
          From ! {'recovered_bytes', Db, (OldDiskSize - NewDiskSize)},
          'ok';
      _ ->
          'ok'
    end;
do_cleanup(From, {Db, _}, _Conn) ->
    From ! {'current_db', Db},
    cleanup_pass(Db),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Listen for events from cleanup/3 process and react upon them. Also take care of
%% answering `auto_compaction_status' requests and wait for `cleanup/3' process to exit
%% to return the state.
%% @end
%%------------------------------------------------------------------------------
-spec loop(kz_term:pid_ref(), auto_compaction()) -> auto_compaction().
loop({Pid, Ref} = PidRef, State) ->
    receive
        {'current_db', Db} ->
            TotalDbs = State#auto_compaction.total_dbs_to_compact,
            Counter = State#auto_compaction.total_dbs_already_compacted + 1,
            lager:debug("compacting ~p out of ~p dbs (~p remaining)",
                        [Counter, TotalDbs, (TotalDbs - Counter)]),
            NewState = State#auto_compaction{'current_db' = Db
                                            ,'total_dbs_already_compacted' = Counter
                                            },
            loop(PidRef, NewState);
        {'recovered_bytes', Db, Recovered} ->
            lager:debug("recovered ~p bytes after compacting ~p db", [Recovered, Db]),
            SoFar = State#auto_compaction.recovered_bytes,
            NewState = State#auto_compaction{'recovered_bytes' = SoFar + Recovered},
            loop(PidRef, NewState);
        {'compaction_status', ReplyTo} ->
            ReplyTo ! {self(), ?RECORD_TO_PROPLIST('auto_compaction', State)},
            loop(PidRef, State);
        {'DOWN', Ref, 'process', Pid, _Info} ->
            %% Finished cleanup_pass. cleanup/0 function exited.
            State#auto_compaction{'current_db' = 'undefined'
                                 ,'running' = 'false'
                                 ,'finished' = kz_time:now_s()
                                 }
    end.

-spec cleanup_pass(kz_term:ne_binary()) -> boolean().
cleanup_pass(Db) ->
    _ = tasks_bindings:map(db_to_trigger(Db), Db),
    erlang:garbage_collect(self()).

-spec db_to_trigger(kz_term:ne_binary()) -> kz_term:ne_binary().
db_to_trigger(Db) ->
    Classifiers = [{fun kapps_util:is_account_db/1, ?TRIGGER_ACCOUNT}
                  ,{fun kapps_util:is_account_mod/1, ?TRIGGER_ACCOUNT_MOD}
                  ,{fun is_system_db/1, ?TRIGGER_SYSTEM}
                  ],
    db_to_trigger(Db, Classifiers).

db_to_trigger(_Db, []) -> ?TRIGGER_OTHER;
db_to_trigger(Db, [{Classifier, Trigger} | Classifiers]) ->
    case Classifier(Db) of
        'true' -> Trigger;
        'false' -> db_to_trigger(Db, Classifiers)
    end.

-spec is_system_db(kz_term:ne_binary()) -> boolean().
is_system_db(Db) ->
    lists:member(Db, ?KZ_SYSTEM_DBS).

-spec get_dbs_disk_and_data(#server{}) -> {[kt_compactor_worker:db_disk_and_data()], pos_integer()}.
get_dbs_disk_and_data(Conn) ->
    {'ok', Dbs} = kz_datamgr:db_info(),
    F = fun(Db, State) -> get_db_disk_and_data_fold(Conn, Db, State, 20) end,
    {DbsSizes, _} = lists:foldl(F, {[], 0}, Dbs),
    DbsSizes.

-spec get_db_disk_and_data_fold(#server{}
                               ,kz_term:ne_binary()
                               ,{[kt_compactor_worker:db_disk_and_data()], non_neg_integer()}
                               , pos_integer()
                               ) -> {[kt_compactor_worker:db_disk_and_data()], pos_integer()}.
get_db_disk_and_data_fold(Conn, UnencDb, {Acc, Counter}, ChunkSize)
  when Counter rem ChunkSize =:= 0 ->
    %% Every `ChunkSize' handled requests, sleep 100ms (give the db a rest).
    timer:sleep(100),
    do_get_db_disk_and_data_fold(Conn, UnencDb, Acc, Counter);
get_db_disk_and_data_fold(Conn, UnencDb, {Acc, Counter}, _ChunkSize) ->
    do_get_db_disk_and_data_fold(Conn, UnencDb, Acc, Counter).

-spec do_get_db_disk_and_data_fold(#server{}
                                  ,kz_term:ne_binary()
                                  ,[kt_compactor_worker:db_disk_and_data()]
                                  ,non_neg_integer()
                                  ) -> {[kt_compactor_worker:db_disk_and_data()], pos_integer()}.
do_get_db_disk_and_data_fold(Conn, UnencDb, Acc, Counter) ->
    EncDb = kz_util:uri_encode(UnencDb),
    {[{UnencDb, kt_compactor_worker:get_db_disk_and_data(Conn, EncDb)} | Acc], Counter + 1}.

-spec sort_by_disk_size(kz_term:proplist()) -> kz_term:proplist().
sort_by_disk_size(DbsSizes) when is_list(DbsSizes) ->
    lists:sort(fun sort_by_disk_size/2, DbsSizes).

-spec sort_by_disk_size({kz_term:ne_binary(), kt_compactor_worker:db_disk_and_data()}
                       ,{kz_term:ne_binary(), kt_compactor_worker:db_disk_and_data()}
                       ) -> boolean().
sort_by_disk_size({_UnencDb1, {DiskSize1, _}}, {_UnencDb2, {DiskSize2, _}}) ->
    DiskSize1 > DiskSize2;
sort_by_disk_size({_UnencDb1, {_DiskSize1, _}}, {_UnencDb2, _Else}) -> %% Else = 'not_found' | 'undefined'
    'true';
sort_by_disk_size({_UnencDb1, _Else}, {_UnencDb2, {_DiskSize2, _}}) -> %% Else = 'not_found' | 'undefined'
    'false'.

-spec save_compaction_job_info(auto_compaction()) -> 'ok'.
save_compaction_job_info(State) ->
    {'ok', AccountId} = kapps_util:get_master_account_id(),
    EncMODB = kz_util:format_account_modb(kz_util:format_account_mod_id(AccountId), 'encoded'),
    JObj = kz_json:from_list(?RECORD_TO_PROPLIST('auto_compaction', State)),
    NewJObj = kz_json:set_values([{<<"pvt_type">>, <<"auto_compaction">>}
                                 ,{<<"pvt_created">>, kz_time:now_s()}
                                 ], JObj),
    {'ok', Doc} = kz_datamgr:save_doc(EncMODB, NewJObj),
    lager:debug("Created Doc after automatic compaction finished: ~p", [Doc]),
    'ok'.

%% =======================================================================================
%% End - Automatic Compaction Section
%% =======================================================================================

%%% End of Module.
