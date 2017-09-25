%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kz_util).

-export([log_stacktrace/0, log_stacktrace/1, log_stacktrace/2

        ,get_running_apps/0
        ,is_kazoo_app/1
        ]).

-export([try_load_module/1]).

-export([uri_encode/1
        ,uri_decode/1
        ,resolve_uri/2
        ]).

-export([uri/2]).

-export([pretty_print_bytes/1, pretty_print_bytes/2
        ,bin_usage/0, mem_usage/0
        ]).

-export([runs_in/3]).
-export([put_callid/1, get_callid/0, find_callid/1
        ,spawn/1, spawn/2
        ,spawn_link/1, spawn_link/2
        ,spawn_monitor/2
        ,set_startup/0, startup/0
        ]).
-export([get_event_type/1]).

-export([kazoo_version/0, write_pid/1]).

-export([change_console_log_level/1
        ,change_error_log_level/1
        ,change_syslog_log_level/1
        ]).

-export([node_name/0, node_hostname/0]).

-export([write_file/2, write_file/3
        ,rename_file/2
        ,delete_file/1
        ,delete_dir/1
        ,make_dir/1
        ]).

-export([calling_app/0]).
-export([calling_app_version/0]).
-export([calling_process/0]).
-export([get_app/1]).

-export([application_version/1]).

-export([uniq/1]).
-export([iolist_join/2]).

-ifdef(TEST).
-export([resolve_uri_path/2]).
-endif.

-include_lib("kernel/include/inet.hrl").

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo/include/kz_api_literals.hrl").

-define(KAZOO_VERSION_CACHE_KEY, {?MODULE, 'kazoo_version'}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Standardized way of logging the stacktrace...
%% @end
%%--------------------------------------------------------------------
-spec log_stacktrace() -> 'ok'.
log_stacktrace() ->
    ST = erlang:get_stacktrace(),
    log_stacktrace(ST).

-spec log_stacktrace(list()) -> ok.
log_stacktrace(ST) ->
    log_stacktrace(ST, "", []).

-spec log_stacktrace(string(), list()) -> ok.
log_stacktrace(Fmt, Args) ->
    ST = erlang:get_stacktrace(),
    log_stacktrace(ST, Fmt, Args).

-ifdef(TEST).
-define(LOG_ERROR(F), io:format(user, "ERROR ~s:~p  " ++ F ++ "\n", [?MODULE,?LINE])).
-define(LOG_ERROR(F,A), io:format(user, "ERROR ~s:~p  " ++ F ++ "\n", [?MODULE,?LINE|A])).
-else.
-define(LOG_ERROR(F,A), lager:error(F,A)).
-define(LOG_ERROR(F), lager:error(F)).
-endif.

log_stacktrace(ST, Fmt, Args) ->
    ?LOG_ERROR("stacktrace: " ++ Fmt, Args),
    _ = [log_stacktrace_mfa(M, F, A, Info)
         || {M, F, A, Info} <- ST
        ],
    'ok'.

log_stacktrace_mfa(M, F, Arity, Info) when is_integer(Arity) ->
    ?LOG_ERROR("st: ~s:~s/~b at (~b)", [M, F, Arity, props:get_value('line', Info, 0)]);
log_stacktrace_mfa(M, F, Args, Info) ->
    ?LOG_ERROR("st: ~s:~s at ~p", [M, F, props:get_value('line', Info, 0)]),
    lists:foreach(fun (Arg) -> ?LOG_ERROR("args: ~p", [Arg]) end, Args).

-define(LOG_LEVELS, ['emergency'
                    ,'alert'
                    ,'critical'
                    ,'error'
                    ,'warning'
                    ,'notice'
                    ,'info'
                    ,'debug'
                    ]).
-type log_level() :: 'emergency'
                   | 'alert'
                   | 'critical'
                   | 'error'
                   | 'warning'
                   | 'notice'
                   | 'info'
                   | 'debug'
                   | ne_binary().

-spec change_console_log_level(log_level()) -> 'ok'.
change_console_log_level(L) when is_atom(L) ->
    lager:info("updated console_log to level ~s", [L]),
    lager:set_loglevel('lager_console_backend', L);
change_console_log_level(L) ->
    change_console_log_level(kz_term:to_atom(L)).

-spec change_error_log_level(log_level()) -> 'ok'.
change_error_log_level(L) when is_atom(L) ->
    lager:info("updated error_log to level ~s", [L]),
    lager:set_loglevel({'lager_file_backend', "log/error.log"}, L);
change_error_log_level(L) ->
    change_error_log_level(kz_term:to_atom(L)).

-spec change_syslog_log_level(log_level()) -> 'ok'.
change_syslog_log_level(L) when is_atom(L) ->
    lager:info("updated syslog_log to level ~s", [L]),
    lager:set_loglevel({'lager_syslog_backend',{"2600hz",'local0'}}, L);
change_syslog_log_level(L) ->
    change_syslog_log_level(kz_term:to_atom(L)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a module name try to verify its existence, loading it into the
%% the vm if possible.
%% @end
%%--------------------------------------------------------------------
-spec try_load_module(atom() | string() | binary()) -> atom() | 'false'.
try_load_module('undefined') -> 'false';
try_load_module("undefined") -> 'false';
try_load_module(<<"undefined">>) -> 'false';
try_load_module(Name) ->
    Module = kz_term:to_atom(Name, 'true'),
    try
        Module:module_info('exports'),
        {'module', Module} = code:ensure_loaded(Module),
        Module
    catch
        'error':'undef' ->
            lager:debug("module ~s not found", [Name]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an JSON Object extracts the Call-ID into the processes
%% dictionary, failing that the Msg-ID and finally a generic
%% @end
%%--------------------------------------------------------------------
-spec put_callid(kz_json:object() | kz_proplist() | ne_binary() | atom()) -> 'ok'.
put_callid(?NE_BINARY = CallId) ->
    lager:md([{'callid', CallId}]),
    erlang:put('callid', CallId),
    'ok';
put_callid(Atom) when is_atom(Atom) ->
    lager:md([{'callid', Atom}]),
    erlang:put('callid', Atom),
    'ok';
put_callid(APITerm) ->
    put_callid(find_callid(APITerm)).

-spec get_callid() -> ne_binary().
get_callid() -> erlang:get('callid').

-spec find_callid(api_terms()) -> api_binary().
find_callid(APITerm) when is_list(APITerm) ->
    find_callid(APITerm, fun props:get_first_defined/3);
find_callid(APITerm) ->
    find_callid(APITerm, fun kz_json:get_first_defined/3).

-spec find_callid(api_terms(), fun()) -> api_binary().
find_callid(APITerm, GetFun) ->
    GetFun([?KEY_LOG_ID, ?KEY_API_CALL_ID, ?KEY_MSG_ID]
          ,APITerm
          ,?LOG_SYSTEM_ID
          ).

%% @public
%% @doc
%% Gives `MaxTime' milliseconds to `Fun' of `Arguments' to apply.
%% If time is elapsed, the sub-process is killed & function returns `timeout'.
%% @end
-spec runs_in(number(), fun(), list()) -> {ok, any()} | timeout.
runs_in(MaxTime, Fun, Arguments)
  when is_integer(MaxTime), MaxTime > 0 ->
    {Parent, Ref} = {self(), erlang:make_ref()},
    Child = ?MODULE:spawn(fun () -> Parent ! {Ref, erlang:apply(Fun, Arguments)} end),
    receive {Ref, Result} -> {ok, Result}
    after MaxTime ->
            exit(Child, kill),
            timeout
    end;
runs_in(MaxTime, Fun, Arguments)
  when is_number(MaxTime), MaxTime > 0 ->
    runs_in(kz_term:to_integer(MaxTime), Fun, Arguments).

-spec spawn(fun(() -> any())) -> pid().
-spec spawn(fun(), list()) -> pid().
spawn(Fun, Arguments) ->
    CallId = get_callid(),
    erlang:spawn(fun() ->
                         _ = put_callid(CallId),
                         erlang:apply(Fun, Arguments)
                 end).
spawn(Fun) ->
    CallId = get_callid(),
    erlang:spawn(fun() ->
                         _ = put_callid(CallId),
                         Fun()
                 end).

-spec spawn_link(fun(() -> any())) -> pid().
-spec spawn_link(fun(), list()) -> pid().
spawn_link(Fun, Arguments) ->
    CallId = get_callid(),
    erlang:spawn_link(fun () ->
                              _ = put_callid(CallId),
                              erlang:apply(Fun, Arguments)
                      end).
spawn_link(Fun) ->
    CallId = get_callid(),
    erlang:spawn_link(fun() ->
                              _ = put_callid(CallId),
                              Fun()
                      end).

-spec spawn_monitor(fun(), list()) -> pid_ref().
spawn_monitor(Fun, Arguments) ->
    CallId = get_callid(),
    erlang:spawn_monitor(fun () ->
                                 _ = put_callid(CallId),
                                 erlang:apply(Fun, Arguments)
                         end).


-spec set_startup() -> api_seconds().
set_startup() ->
    put('$startup', kz_time:now_s()).

-spec startup() -> api_seconds().
startup() ->
    get('$startup').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an object, extract the category and name into a tuple
%% @end
%%--------------------------------------------------------------------
-spec get_event_type(api_terms()) -> {api_binary(), api_binary()}.
get_event_type(Props) when is_list(Props) ->
    {props:get_value(<<"Event-Category">>, Props)
    ,props:get_value(<<"Event-Name">>, Props)
    };
get_event_type(JObj) ->
    {kz_json:get_value(<<"Event-Category">>, JObj)
    ,kz_json:get_value(<<"Event-Name">>, JObj)
    }.

-spec uri_decode(text()) -> text().
uri_decode(Binary) when is_binary(Binary) ->
    kz_term:to_binary(http_uri:decode(kz_term:to_list(Binary)));
uri_decode(String) when is_list(String) ->
    http_uri:decode(String);
uri_decode(Atom) when is_atom(Atom) ->
    kz_term:to_atom(http_uri:decode(kz_term:to_list(Atom)), 'true').

-spec uri_encode(text()) -> text().
uri_encode(Binary) when is_binary(Binary) ->
    kz_term:to_binary(http_uri:encode(kz_term:to_list(Binary)));
uri_encode(String) when is_list(String) ->
    http_uri:encode(String);
uri_encode(Atom) when is_atom(Atom) ->
    kz_term:to_atom(http_uri:encode(kz_term:to_list(Atom)), 'true').

-spec resolve_uri(nonempty_string() | ne_binary(), nonempty_string() | api_ne_binary()) -> ne_binary().
resolve_uri(Raw, 'undefined') -> kz_term:to_binary(Raw);
resolve_uri(_Raw, <<"http", _/binary>> = Abs) -> Abs;
resolve_uri(<<_/binary>> = RawPath, <<_/binary>> = Relative) ->
    Path = resolve_uri_path(RawPath, Relative),
    kz_binary:join(Path, <<"/">>);
resolve_uri(RawPath, Relative) ->
    resolve_uri(kz_term:to_binary(RawPath), kz_term:to_binary(Relative)).

-spec resolve_uri_path(ne_binary(), ne_binary()) -> ne_binaries().
resolve_uri_path(RawPath, Relative) ->
    PathTokensRev = lists:reverse(binary:split(RawPath, <<"/">>, ['global'])),
    UrlTokens = binary:split(Relative, <<"/">>, ['global']),
    lists:reverse(
      lists:foldl(fun resolve_uri_fold/2, PathTokensRev, UrlTokens)
     ).

-spec resolve_uri_fold(ne_binary(), ne_binaries()) -> ne_binaries().
resolve_uri_fold(<<"..">>, []) -> [];
resolve_uri_fold(<<"..">>, [_ | PathTokens]) -> PathTokens;
resolve_uri_fold(<<".">>, PathTokens) -> PathTokens;
resolve_uri_fold(<<>>, PathTokens) -> PathTokens;
resolve_uri_fold(Segment, [<<>>|DirTokens]) -> [Segment|DirTokens];
resolve_uri_fold(Segment, [LastToken|DirTokens]=PathTokens) ->
    case filename:extension(LastToken) of
        <<>> ->
            %% no extension, append Segment to Tokens
            [Segment | PathTokens];
        _Ext ->
            %% Extension found, append Segment to DirTokens
            [Segment|DirTokens]
    end.

-spec uri(ne_binary(), ne_binaries()) -> ne_binary().
uri(BaseUrl, Tokens) ->
    [Pro, Url] = binary:split(BaseUrl, <<"://">>),
    Uri = filename:join([Url | Tokens]),
    <<Pro/binary, "://", Uri/binary>>.

%% fetch and cache the kazoo version from the VERSION file in kazoo's root folder
-spec kazoo_version() -> ne_binary().
kazoo_version() ->
    {_, _, Version} = get_app('kazoo'),
    kz_term:to_binary(Version).

-spec write_pid(file:filename_all()) -> 'ok' | {'error', atom()}.
write_pid(FileName) ->
    file:write_file(FileName, io_lib:format("~s", [os:getpid()]), ['write', 'binary']).


-spec pretty_print_bytes(non_neg_integer()) -> ne_binary().
-spec pretty_print_bytes(non_neg_integer(), 'full' | 'truncated') -> ne_binary().
pretty_print_bytes(Bytes) ->
    pretty_print_bytes(Bytes, 'full').

pretty_print_bytes(0, _) -> <<"0B">>;
pretty_print_bytes(Bytes, Type) ->
    iolist_to_binary(unitfy_bytes(Bytes, Type)).

-spec unitfy_bytes(non_neg_integer(), 'full' | 'truncated') -> iolist().
unitfy_bytes(0, _Type) -> "";
unitfy_bytes(Bytes, _Type) when Bytes < ?BYTES_K  ->
    [kz_term:to_binary(Bytes), "B"];
unitfy_bytes(Bytes, Type) when Bytes < ?BYTES_M ->
    K = Bytes div ?BYTES_K,
    [kz_term:to_binary(K), "K", maybe_unitfy_bytes(Bytes rem ?BYTES_K, Type)];
unitfy_bytes(Bytes, Type) when Bytes < ?BYTES_G ->
    M = Bytes div ?BYTES_M,
    [kz_term:to_binary(M), "M", maybe_unitfy_bytes(Bytes rem ?BYTES_M, Type)];
unitfy_bytes(Bytes, Type) when Bytes < ?BYTES_T ->
    G = Bytes div ?BYTES_G,
    [kz_term:to_binary(G), "G", maybe_unitfy_bytes(Bytes rem ?BYTES_G, Type)];
unitfy_bytes(Bytes, Type) ->
    T = Bytes div ?BYTES_T,
    [kz_term:to_binary(T), "T", maybe_unitfy_bytes(Bytes rem ?BYTES_T, Type)].

-spec maybe_unitfy_bytes(non_neg_integer(), 'full' | 'truncated') -> iolist().
maybe_unitfy_bytes(Bytes, 'full'=Type) ->
    unitfy_bytes(Bytes, Type);
maybe_unitfy_bytes(_Bytes, 'truncated') ->
    <<>>.

-spec bin_usage() -> integer().
bin_usage() ->
    {'ok', {_, Usage, _}} = recon_lib:proc_attrs(binary_memory, self()),
    Usage.

-spec mem_usage() -> integer().
mem_usage() ->
    {'memory', Memory} = erlang:process_info(self(), 'memory'),
    Memory.

-spec node_name() -> binary().
-spec node_hostname() -> binary().
node_name() ->
    [Name, _Host] = binary:split(kz_term:to_binary(node()), <<"@">>),
    Name.
node_hostname() ->
    [_Name, Host] = binary:split(kz_term:to_binary(node()), <<"@">>),
    Host.

%% @public
-spec write_file(file:filename_all(), iodata()) -> 'ok'.
write_file(Filename, Bytes) ->
    write_file(Filename, Bytes, []).

%% @public
-spec write_file(file:filename_all(), iodata(), [file:mode()]) -> 'ok'.
write_file(Filename, Bytes, Modes) ->
    case file:write_file(Filename, Bytes, Modes) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:error("writing file ~s (~p) failed : ~p", [Filename, Modes, _E])
    end.

-spec rename_file(file:filename_all(), file:filename_all()) -> 'ok'.
rename_file(FromFilename, ToFilename) ->
    case file:rename(FromFilename, ToFilename) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:error("moving file ~s into ~s failed : ~p", [FromFilename, ToFilename, _E])
    end.

%% @public
-spec delete_file(file:filename_all()) -> 'ok'.
delete_file(Filename) ->
    case file:delete(Filename) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:error("deleting file ~s failed : ~p", [Filename, _E])
    end.

%% @public
-spec delete_dir(string()) -> 'ok'.
delete_dir(Dir) ->
    F = fun(D) -> 'ok' = file:del_dir(D) end,
    lists:foreach(F, del_all_files([Dir], [])).

-spec del_all_files(strings(), strings()) -> strings().
del_all_files([], EmptyDirs) -> EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
    {'ok', FilesInDir} = file:list_dir(Dir),
    {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                        Path = Dir ++ "/" ++ F,
                                        case filelib:is_dir(Path) of
                                            'true' ->
                                                {Fs, [Path | Ds]};
                                            'false' ->
                                                {[Path | Fs], Ds}
                                        end
                                end, {[],[]}, FilesInDir),
    lists:foreach(fun delete_file/1, Files),
    del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

%% @public
-spec make_dir(file:filename_all()) -> 'ok'.
make_dir(Filename) ->
    case file:make_dir(Filename) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:error("creating directory ~s failed : ~p", [Filename, _E])
    end.

-spec process_fold([tuple()], atom()) -> tuple() | atom().
process_fold([], App) -> App;
process_fold([{M, _, _, _}=Mod | Others], App) ->
    ModApp = case application:get_application(M) of
                 {'ok', KModApp} -> KModApp;
                 'undefined' -> M
             end,
    process_fold(ModApp, App, Mod, Others).

-spec process_fold(atom(), atom(), tuple(), [tuple()]) -> tuple() | atom().
process_fold(App, App, _, Others) ->
    process_fold(Others, App);
process_fold(App, _, M, _) -> {App, M}.

%% @doc
%% For core apps that want to know which app is calling.
%% @end
-spec calling_app() -> ne_binary().
calling_app() ->
    Modules = erlang:process_info(self(),current_stacktrace),
    {'current_stacktrace', [_Me, {Module, _, _, _} | Start]} = Modules,
    {'ok', App} = application:get_application(Module),
    case process_fold(Start, App) of
        App -> kz_term:to_binary(App);
        {Parent, _MFA} -> kz_term:to_binary(Parent)
    end.

-spec calling_app_version() -> {ne_binary(), ne_binary()}.
calling_app_version() ->
    Modules = erlang:process_info(self(),current_stacktrace),
    {'current_stacktrace', [_Me, {Module, _, _, _} | Start]} = Modules,
    {'ok', App} = application:get_application(Module),
    NewApp = case process_fold(Start, App) of
                 App -> App;
                 {Parent, _MFA} -> Parent
             end,
    {NewApp, _, Version} = get_app(NewApp),
    {kz_term:to_binary(NewApp), kz_term:to_binary(Version)}.

-spec calling_process() -> map().
calling_process() ->
    Modules = erlang:process_info(self(),current_stacktrace),
    {'current_stacktrace', [_Me, {Module, _, _, _}=M | Start]} = Modules,
    App = case application:get_application(Module) of
              {'ok', KApp} -> KApp;
              'undefined' -> Module
          end,
    {NewApp, {Mod, Function, Arity, [{file, Filename}, {line, Line}]}} =
        case process_fold(Start, App) of
            App -> {App, M};
            {Parent, MFA } -> {Parent, MFA}
        end,
    #{app => NewApp
     ,module => Mod
     ,function => Function
     ,arity => Arity
     ,file => Filename
     ,line => Line
     }.

-spec get_app(atom() | ne_binary()) -> {atom(), string(), string()} | 'undefined'.
get_app(<<_/binary>> = AppName) ->
    get_app(kz_term:to_atom(AppName));
get_app(AppName) ->
    case [App || {Name, _, _}=App <- application:loaded_applications(), Name =:= AppName] of
        [] -> 'undefined';
        [Ret | _] -> Ret
    end.

-spec application_version(atom()) -> ne_binary().
application_version(Application) ->
    {'ok', Vsn} = application:get_key(Application, 'vsn'),
    kz_term:to_binary(Vsn).

%% @doc
%% Like lists:usort/1 but preserves original ordering.
%% Time: O(nlog(n)).
%% @end
-spec uniq([kz_proplist()]) -> kz_proplist().
uniq(KVs) when is_list(KVs) -> uniq(KVs, sets:new(), []).
uniq([], _, L) -> lists:reverse(L);
uniq([{K,_}=KV|Rest], S, L) ->
    case sets:is_element(K, S) of
        true -> uniq(Rest, S, L);
        false ->
            NewS = sets:add_element(K, S),
            uniq(Rest, NewS, [KV|L])
    end.

%% @public
-spec iolist_join(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
      T :: iodata() | char().
iolist_join(_, []) -> [];
iolist_join(Sep, [H|T]) ->
    [H | iolist_join_prepend(Sep, T)].

%% @private
-spec iolist_join_prepend(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
      T :: iolist().
iolist_join_prepend(_, []) -> [];
iolist_join_prepend(Sep, [H|T]) ->
    [Sep, H | iolist_join_prepend(Sep, T)].

-spec get_running_apps() -> [{atom(), string(), _}].
get_running_apps() ->
    [AppData
     || {App, _Desc, _Vsn}=AppData <- application:which_applications(),
        is_kazoo_app(App)
    ].

-spec is_kazoo_app(atom()) -> boolean().
is_kazoo_app(App) ->
    case application:get_key(App, 'applications') of
        {'ok', Deps} -> lists:member('kazoo_apps', Deps);
        'undefined' ->
            %% Race condition sometimes prevents from reading application key
            'non_existing' =/= code:where_is_file(atom_to_list(App) ++ ".app")
    end.
