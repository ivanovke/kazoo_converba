#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(CommandLineArgs) ->
    add_ebins(),
    {'ok', Options, Args} = parse_args(CommandLineArgs),
    io:format("cwd: ~p~no: ~p~na: ~p~n", [file:get_cwd(), Options, Args]),
    handle(Options, Args).

%% handle([], []) ->
%%     kast_app_deps:fix_project_deps();
handle(['app'], [App]) ->
    kast_app_deps:fix_app_deps(list_to_atom(App));
handle(['dot'], []) ->
    kast_app_deps:dot_file();
handle(['dot'], [App]) ->
    kast_app_deps:dot_file(list_to_atom(App));
handle([], [App]) ->
    list_remote_apps(App);
handle(_, _) ->
    print_help().

list_remote_apps(App) ->
    try kast_app_deps:remote_apps(list_to_atom(App)) of
        Apps ->
            io:format("~n~p~n", [lists:keysort(2, Apps)])
    catch
        'error':{"no such file or directory",AppFile} ->
            io:format("failed to find ~s (as part of ~s)~n", [AppFile, App]),
            add_core_ebins(list_to_atom(App)),
            list_remote_apps(App)
    end.


%%     case kast_app_deps:process_project() of
%%         [] -> 'ok';
%%         BadApps ->
%%             [output_bad_app(A, Missing, Needed)
%%              || {A, Missing, Needed} <- BadApps
%%             ],
%%             erlang:halt(1)
%%     end.

%% output_bad_app(App, MissingApps, UnneededApps) ->
%%     io:format("application ~s has discrepancies in its ~s.app file:~n", [App, App]),
%%     MissingApps =/= []
%%         andalso io:format("  apps missing from the list: ~s~n", [kz_util:join_binary(MissingApps)]),
%%     UnneededApps =/= []
%%         andalso io:format("  unnecessary apps: ~s~n", [kz_util:join_binary(UnneededApps)]),
%%     io:format("~n").

-spec option_spec_list() -> list().
option_spec_list() ->
    [{'help', $?, "help", 'undefined', "Show the program options"}
    ,{'app', $a, "app", 'undefined', "The app to process"}
    ,{'dot', $d, "dot", 'undefined', "Generate a DOT file"}
    ].

-spec parse_args(string()) -> {'ok', list(), list()}.
parse_args(CommandLineArgs) ->
    case getopt:parse(option_spec_list(), CommandLineArgs) of
        {'ok', {Options, Args}} when is_list(Options) ->
            {'ok', Options, Args};
        {'ok', {_, _}} ->
            print_help();
        {'error', {_, _}} ->
            print_help()
    end.

-spec print_help() -> no_return().
print_help() ->
    getopt:usage(option_spec_list(), "apps_of_app", "[args ...]"),
    halt(1).

add_ebins() ->
    add_deps_ebins(),
    add_core_ebins().

add_deps_ebins() ->
    add_deps_ebins(deps()).
add_deps_ebins(Deps) ->
    DepsDir = filename:join([filename:dirname(escript:script_name())
                            ,".." %% root
                            ,"deps"
                            ]),
    _ = [code:add_patha(dep_dir(DepsDir, Dep)) || Dep <- Deps].

add_core_ebins() ->
    add_core_ebins(core()).
add_core_ebins(Core) ->
    CoreDir = filename:join([filename:dirname(escript:script_name())
                            ,".." %% root
                            ,"core"
                            ]),

    _ = [code:add_patha(dep_dir(CoreDir, Dep)) || Dep <- Core].

dep_dir(DepsDir, Dep) ->
    filename:join([DepsDir
                  ,atom_to_list(Dep)
                  ,"ebin"
                  ]).

deps() ->
    ['getopt'].

core() ->
    ['kazoo', 'kazoo_ast'].
