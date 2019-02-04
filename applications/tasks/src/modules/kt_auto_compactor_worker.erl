-module(kt_auto_compactor_worker).
-behaviour(gen_server).

-include_lib("couchbeam/include/couchbeam.hrl"). %% Contains #server{} record definition

-export([start_link/0]).
%% Handlers for automatic compaction SUP commands
-export([status/0, history/0, history/1, notify_skipped_db/1]).

-ifdef(TEST).
-export([sort_by_disk_size/1
        ]).
-endif.

-define(SERVER, {'via', 'kz_globals', ?MODULE}).
-define(AUTO_COMPACTION_VIEW, <<"auto_compaction/crossbar_listing">>).

-type db_disk_and_data() :: {kz_term:ne_binary(), kt_compactor_worker:db_disk_and_data()}.
-record('state', {%% Databases
                 'id' :: kz_term:ne_binary()
                 ,'found' :: pos_integer() %% Number of dbs found to be compacted
                 ,'compacted' = 0 :: non_neg_integer() %% Number of dbs compacted so far
                 ,'queued' = 0 :: non_neg_integer() %% remaining dbs to be compacted
                 ,'failures' = 0 :: non_neg_integer() %% Number of 'not_found' and 'undefined' got when asking for disk and data size
                 ,'skipped' = 0 :: non_neg_integer() %% dbs skipped because not data_size nor disk-data's ratio thresholds are met.
                 ,'current_db' = 'undefined' :: kz_term:ne_binary()
                 ,'dbs_and_info' = [] :: [db_disk_and_data()] %% List of dbs and their disk and data sizes (sorted by disk_size desc).
                 %% Storage
                 ,'disk_start' = 0 :: non_neg_integer() %% disk_size sum of all dbs in bytes before compaction (for history sup command)
                 ,'disk_end' = 0 :: non_neg_integer() %% disk_size sum of all dbs in bytes after compaction (for history sup command)
                 ,'data_start' = 0 :: non_neg_integer() %% data_size sum of all dbs in bytes before compaction (for history sup command)
                 ,'data_end' = 0 :: non_neg_integer() %% data_size sum of all dbs in bytes after compaction (for history sup command)
                 ,'recovered_disk' = 0 :: non_neg_integer() %% bytes recovered so far (auto_compaction status command)
                 %% Worker
                 ,'pid' = self() :: pid() %% worker's pid
                 ,'node' = node() :: node() %% node where the worker is running
                 ,'started' = kz_time:now_s() :: kz_time:gregorian_seconds() %% datetime (in seconds) when the compaction started
                 ,'finished' = 'undefined' :: kz_time:gregorian_seconds() %% datetime (in seconds) when the compaction ended
                 ,'cleanup_pid_ref' = 'undefined' :: {pid(), reference()}
                 }).
-type state() :: #state{}.


%%%=============================================================================
%%% API
%%%=============================================================================

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

%%------------------------------------------------------------------------------
%% @doc Notifies when a database has been skipped by the compactor worker. This happens
%% when not data_size nor disk-data's ratio thresholds are met.
%% @end
%%------------------------------------------------------------------------------
-spec notify_skipped_db(kz_term:ne_binary()) -> 'ok'.
notify_skipped_db(Db) when is_binary(Db) ->
    gen_server:cast(?SERVER, {'skipped_db', Db}).

%%------------------------------------------------------------------------------
%% @doc SUP command handler
%% @end
%%------------------------------------------------------------------------------
-spec status() -> 'ok'.
status() ->
    try
        Props = gen_server:call(?SERVER, 'status'),
        lists:foreach(fun({K, V}) -> io:format("~s: ~s~n", [K, V]) end, Props)
    catch
        {'noproc', _} ->
            io:format("not running~n")
    end.

%%------------------------------------------------------------------------------
%% @doc SUP command handler. Returns history for the current Year and Month.
%% @end
%%------------------------------------------------------------------------------
history() ->
    {Year, Month, _} = erlang:date(),
    history(Year, Month).

%%------------------------------------------------------------------------------
%% @doc SUP command handler. Accepts `YYYYMM' or `YYYYM' as param to request history
%% for the given Year and Month.
%% @end
%%------------------------------------------------------------------------------
-spec history(kz_term:ne_binary()) -> 'ok'.
history(<<Year:4/binary, Month:2/binary>>) ->
    history(kz_term:to_integer(Year), kz_term:to_integer(Month));
history(<<Year:4/binary, Month:1/binary>>) ->
    history(kz_term:to_integer(Year), kz_term:to_integer(Month));
history(Else) ->
    io:format("Invalid argument *~p*~n", [Else]).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    CallId = <<"cleanup_pass_", (kz_binary:rand_hex(4))/binary>>,
    kz_util:put_callid(CallId),
    lager:debug("starting cleanup pass of databases"),
    %% Return early, actual initialization will happen at `handle_info(timeout...'
    %% because listing all dbs from bigcouch and then get their disk and data sizes
    %% can take a lot of time.
    {'ok', #state{'id' = CallId}, 0}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('status', _From, #state{'found' = Found
                                   ,'failures' = Failed
                                   ,'skipped' = Skipped
                                   } = State) ->
    Compacted = Found - Failed - Skipped,
    Ret = [{<<"id">>, kz_term:to_binary(State#state.id)}
          ,{<<"found">>, kz_term:to_binary(Found)}
          ,{<<"compacted">>, kz_term:to_binary(Compacted)}
          ,{<<"queued">>, kz_term:to_binary(State#state.queued)}
          ,{<<"failures">>, kz_term:to_binary(Failed)}
          ,{<<"skipped">>, kz_term:to_binary(Skipped)}
          ,{<<"current_db">>, kz_term:to_binary(State#state.current_db)}
          ,{<<"recovered_disk">>, kz_term:to_binary(State#state.recovered_disk)}
          ,{<<"pid">>, kz_term:to_binary(State#state.pid)}
          ,{<<"node">>, kz_term:to_binary(State#state.node)}
          ,{<<"started">>, kz_term:to_binary(State#state.started)}
          ],
    {'reply', Ret, State};

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call ~p from ~p", [_Request, _From]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'skipped_db', Db}, State = #state{'skipped' = Skipped}) ->
    lager:debug("~p db does not need compaction, skipped", [Db]),
    {'noreply', State#state{'skipped' = Skipped + 1}};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('timeout', #state{'dbs_and_info' = []} = State) ->
    %% Continue initialization process from init/1
    #{'server' := {_App, #server{}=Conn}} = kzs_plan:plan(),
    {DbsAndSizes, DiskStart, DataStart, _} = get_dbs_disk_and_data(Conn),
    Sorted = sort_by_disk_size(DbsAndSizes),
    TotalSorted = length(Sorted),
    CleanupPidRef = kz_util:spawn_monitor(fun cleanup/3, [self(), Sorted, Conn]),
    {'noreply', #state{'found' = TotalSorted
                      ,'queued' = TotalSorted
                      ,'disk_start' = DiskStart
                      ,'data_start' = DataStart
                      ,'cleanup_pid_ref' = CleanupPidRef
                      }};

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
%% @doc Print automatic compaction history according to user's input.
%% @end
%%------------------------------------------------------------------------------
-spec history(kz_time:year(), kz_time:month()) -> 'ok'.
history(Year, Month) ->
    {'ok', AccountId} = kapps_util:get_master_account_id(),
    Opts = [{'year', Year}, {'month', Month}, 'include_docs'],
    case kazoo_modb:get_results(AccountId, ?AUTO_COMPACTION_VIEW, Opts) of
        {'ok', []} ->
            io:format("Not history found for ~p-~p (year-month)~n", [Year, Month]);
        {'ok', JObjs} ->
            Header = ["id"
                     ,"found"
                     ,"compacted"
                     ,"skipped"
                     ,"failed"
                     ,"recovered"
                     ,"started_at"
                     ,"finished_at"
                     ,"exec_time"
                     ],
            %% Last # is because the count of | (pipe) chars within `FStr' string.
            HLine = lists:flatten(lists:duplicate(21+6+9+6+6+15+19+19+12+10, "-")),
            %% Format string for printing header and values of the table including "columns".
            FStr = "|~.21s|~.6s|~.9s|~.6s|~.6s|~.15s|~.19s|~.19s|~.12s|~n",
            %% Print top line of table, then prints the header and then another line below.
            io:format("~s~n" ++ FStr ++ "~s~n", [HLine] ++ Header ++ [HLine]),
            lists:foreach(fun(Obj) -> print_history_row(Obj, FStr) end, JObjs),
            io:format("~s~n", [HLine])
    end.

-spec print_history_row(kz_json:object(), string()) -> string().
print_history_row(JObj, FStr) ->
    Doc = kz_json:get_json_value(<<"doc">>, JObj),
    Str = fun(K, From) -> kz_json:get_string_value(K, From) end,
    Int = fun(K, From) -> kz_json:get_integer_value(K, From) end,
    StartInt = Int([<<"worker">>, <<"started">>], Doc),
    EndInt = Int([<<"worker">>, <<"finished">>], Doc),
    Row = [Str(<<"_id">>, Doc)
          ,Str([<<"databases">>, <<"found">>], Doc)
          ,Str([<<"databases">>, <<"compacted">>], Doc)
          ,Str([<<"databases">>, <<"skipped">>], Doc)
          ,Str([<<"databases">>, <<"failures">>], Doc)
          ,kz_util:pretty_print_bytes(Int([<<"storage">>, <<"disk">>, <<"start">>], Doc) -
                                      Int([<<"storage">>, <<"disk">>, <<"end">>], Doc))
          ,kz_term:to_list(kz_time:pretty_print_datetime(StartInt))
          ,kz_term:to_list(kz_time:pretty_print_datetime(EndInt))
          ,kz_term:to_list(kz_time:pretty_print_elapsed_s(EndInt - StartInt))
          ],
    io:format(FStr, Row).

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
    save_auto_compaction_job_info(NewState).

-spec cleanup(pid()
             ,[{kz_term:ne_binary(), kt_compactor_worker:db_disk_and_data()}]
             ,#server{}
             ) -> 'ok'.
cleanup(From, Sorted, Conn) ->
    lists:foreach(fun(Db) -> do_cleanup(From, Db, Conn) end, Sorted).

%%------------------------------------------------------------------------------
%% @doc Trigger db compaction for found dbs
%%
%% Recovered bytes can ONLY be calculated for dbs that include sizes, e.g:
%% `{DbName, {DiskSize, DataSize}}'
%%
%% Dbs WITHOUT sizes only trigger `current_db' event.
%% @end
%%------------------------------------------------------------------------------
-spec do_cleanup(pid()
                ,{kz_term:ne_binary(), kt_compactor_worker:db_disk_and_data()}
                ,#server{}
                ) -> 'ok'.
do_cleanup(Handler, {Db, {OldDisk, OldData}}, Conn) ->
    EncDb = kz_util:uri_encode(Db),
    gen_server:cast(?SERVER, {'current_db', Db}),
    cleanup_pass(Db),
    case kt_compactor_worker:get_db_disk_and_data(Conn, EncDb) of
        {NewDisk, NewData} ->
            gen_server:cast(?SERVER,
                            {'finished_db', Db, OldDisk, OldData, NewDisk, NewData}),
            'ok';
        _ ->
            %% If not sizes found then it is not possible to calculate recovered bytes
            'ok'
    end;
do_cleanup(Handler, {Db, _}, _Conn) -> %% _ = 'not_found' | 'undefined' disk and data sizes
    gen_server:cast(?SERVER, {'current_db', Db}),
    %% Failure because it couldn't get disk and data sizes at the beginning of the job.
    gen_server:cast(?SERVER, {'failure_db', Db}),
    cleanup_pass(Db),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Listen for events from cleanup/3 process and react upon them. Also take care of
%% answering `auto_compaction_status' requests and wait for `cleanup/3' process to exit
%% to return the state.
%% @end
%%------------------------------------------------------------------------------
-spec loop(kz_term:pid_ref(), auto_compaction()) -> auto_compaction().
loop(PidRef, State) ->
    receive
        Event -> process_event(Event, PidRef, State)
    end.

-spec process_event( {'current_db' | 'failure_db' | 'db_skipped', kz_term:ne_binary()}
                    |{'finished_db'
                     ,kz_term:ne_binary()
                     ,non_neg_integer()
                     ,non_neg_integer()
                     ,non_neg_integer()
                     ,non_neg_integer()
                     }
                    |{'compaction_status', pid()}
                    |{'DOWN', reference(), 'process', pid(), any()}
                   ,kz_term:pid_ref()
                   ,auto_compaction()
                   ) -> auto_compaction().
process_event({'current_db', Db}, PidRef, State) ->
    Found = State#auto_compaction.found,
    Compacted = State#auto_compaction.compacted + 1,
    Queued = State#auto_compaction.queued - 1,
    lager:debug("compacting ~p out of ~p dbs (~p remaining)", [Compacted, Found, Queued]),
    loop(PidRef, State#auto_compaction{'current_db' = Db});
process_event({'finished_db', Db, OldDisk, _OldData, NewDisk, NewData}, PidRef, State) ->
    CurrentRec = State#auto_compaction.recovered_disk,
    DiskEnd = State#auto_compaction.disk_end,
    DataEnd = State#auto_compaction.data_end,
    Compacted = State#auto_compaction.compacted,
    Queued = State#auto_compaction.queued,
    Recovered = (OldDisk-NewDisk),
    lager:debug("recovered ~p bytes after compacting ~p db", [Recovered, Db]),
    loop(PidRef,
         State#auto_compaction{'recovered_disk' = CurrentRec + Recovered
                              ,'disk_end' = DiskEnd + NewDisk
                              ,'data_end' = DataEnd + NewData
                              ,'compacted' = Compacted + 1
                              ,'queued' = Queued - 1
                              });
process_event({'failure_db', _}, PidRef, #auto_compaction{'failures' = Fails} = State) ->
    %% Db without disk and data sizes (was not possible to get these sizes when auto compaction started).
    loop(PidRef, State#auto_compaction{'failures' = Fails + 1});
process_event({'compaction_status', From}, PidRef, State) ->
    Ret = [{<<"id">>, kz_term:to_binary(State#auto_compaction.id)}
          ,{<<"found">>, kz_term:to_binary(State#auto_compaction.found)}
          ,{<<"compacted">>, kz_term:to_binary(State#auto_compaction.compacted)}
          ,{<<"queued">>, kz_term:to_binary(State#auto_compaction.queued)}
          ,{<<"failures">>, kz_term:to_binary(State#auto_compaction.failures)}
          ,{<<"skipped">>, kz_term:to_binary(State#auto_compaction.skipped)}
          ,{<<"current_db">>, kz_term:to_binary(State#auto_compaction.current_db)}
          ,{<<"recovered_disk">>, kz_term:to_binary(State#auto_compaction.recovered_disk)}
          ,{<<"pid">>, kz_term:to_binary(State#auto_compaction.pid)}
          ,{<<"node">>, kz_term:to_binary(State#auto_compaction.node)}
          ,{<<"started">>, kz_term:to_binary(State#auto_compaction.started)}
          ],
    From ! {self(), Ret},
    loop(PidRef, State);
process_event({'DOWN', Ref, 'process', Pid, _Info}, {Pid, Ref}, State) ->
    %% Finished cleanup_pass. cleanup/0 function exited.
    State#auto_compaction{'current_db' = 'undefined'
                         ,'finished' = kz_time:now_s()
                         }.

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

-spec get_dbs_disk_and_data(#server{}) -> {[db_disk_and_data()]
                                          ,pos_integer() %% disk_size (bytes) start
                                          ,pos_integer() %% data_size (bytes) start
                                          ,pos_integer() %% counter used by the fold
                                          }.
get_dbs_disk_and_data(Conn) ->
    {'ok', Dbs} = kz_datamgr:db_info(),
    F = fun(Db, State) -> get_db_disk_and_data_fold(Conn, Db, State, 20) end,
    lists:foldl(F, {[], 0, 0, 0}, Dbs).

-spec get_db_disk_and_data_fold(#server{}
                               ,kz_term:ne_binary()
                               ,{[db_disk_and_data()]
                                ,non_neg_integer()
                                ,non_neg_integer()
                                ,non_neg_integer()
                                }
                               , pos_integer()
                               ) -> {[db_disk_and_data()]
                                    ,pos_integer()
                                    ,pos_integer()
                                    ,pos_integer()
                                    }.
get_db_disk_and_data_fold(Conn, UnencDb, {_, _, _, Counter} = State, ChunkSize)
  when Counter rem ChunkSize =:= 0 ->
    %% Every `ChunkSize' handled requests, sleep 100ms (give the db a rest).
    timer:sleep(100),
    do_get_db_disk_and_data_fold(Conn, UnencDb, State);
get_db_disk_and_data_fold(Conn, UnencDb, State, _ChunkSize) ->
    do_get_db_disk_and_data_fold(Conn, UnencDb, State).

-spec do_get_db_disk_and_data_fold(#server{}
                                  ,kz_term:ne_binary()
                                  ,{[db_disk_and_data()]
                                   ,non_neg_integer()
                                   ,non_neg_integer()
                                   ,non_neg_integer()
                                   }
                                  ) -> {[db_disk_and_data()]
                                       ,pos_integer()
                                       ,pos_integer()
                                       ,pos_integer()
                                       }.
do_get_db_disk_and_data_fold(Conn, UnencDb, {Acc, DiskStart, DataStart, Counter}) ->
    EncDb = kz_util:uri_encode(UnencDb),
    DiskAndData = kt_compactor_worker:get_db_disk_and_data(Conn, EncDb),
    {NewDisk, NewData} = case DiskAndData of
                             {Disk, Data} -> {DiskStart+Disk, DataStart+Data};
                             _ -> {DiskStart, DataStart} %% 'not_found' or 'undefined'
                         end,
    {[{UnencDb, DiskAndData} | Acc], NewDisk, NewData, Counter + 1}.

-spec sort_by_disk_size([db_disk_and_data()]) -> [db_disk_and_data()].
sort_by_disk_size(DbsSizes) when is_list(DbsSizes) ->
    lists:sort(fun sort_by_disk_size/2, DbsSizes).

-spec sort_by_disk_size(db_disk_and_data(), db_disk_and_data()) -> boolean().
sort_by_disk_size({_UnencDb1, {DiskSize1, _}}, {_UnencDb2, {DiskSize2, _}}) ->
    DiskSize1 > DiskSize2;
sort_by_disk_size({_UnencDb1, {_DiskSize1, _}}, {_UnencDb2, _Else}) -> %% Else = 'not_found' | 'undefined'
    'true';
sort_by_disk_size({_UnencDb1, _Else}, {_UnencDb2, {_DiskSize2, _}}) -> %% Else = 'not_found' | 'undefined'
    'false'.

-spec save_auto_compaction_job_info(auto_compaction()) -> 'ok'.
save_auto_compaction_job_info(State) ->
    Found = State#auto_compaction.found,
    Failures = State#auto_compaction.failures,
    Skipped = State#auto_compaction.skipped,
    Compacted = Found - Failures - Skipped,
    Map = #{<<"_id">> => State#auto_compaction.id
           ,<<"databases">> => #{<<"found">> => Found
                                ,<<"compacted">> => Compacted
                                ,<<"queued">> => State#auto_compaction.queued
                                ,<<"failures">> => Failures
                                ,<<"skipped">> => Skipped
                                }
           ,<<"storage">> => #{<<"disk">> =>
                                 #{<<"start">> => State#auto_compaction.disk_start
                                  ,<<"end">> => State#auto_compaction.disk_end
                                  }
                              ,<<"data">> =>
                                 #{<<"start">> => State#auto_compaction.data_start
                                  ,<<"end">> => State#auto_compaction.data_end
                                  }
                              }
           ,<<"worker">> => #{<<"pid">> => kz_term:to_binary(State#auto_compaction.pid)
                             ,<<"node">> => kz_term:to_binary(State#auto_compaction.node)
                             ,<<"started">> => State#auto_compaction.started
                             ,<<"finished">> => State#auto_compaction.finished
                             }
           ,<<"pvt_type">> => <<"auto_compaction">>
           ,<<"pvt_created">> => kz_time:now_s()
           },
    lager:debug("Saving stats after automatic compaction completion: ~p", [Map]),
    {'ok', AccountId} = kapps_util:get_master_account_id(),
    {'ok', Doc} = kazoo_modb:save_doc(AccountId, kz_json:from_map(Map)),
    lager:debug("Created doc after automatic compaction completion: ~p", [Doc]),
    'ok'.


