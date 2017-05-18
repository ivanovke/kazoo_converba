#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(CommandLineArgs) ->
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
handle(['project'], []) ->
    kast_app_deps:fix_project_deps();
handle(['module'], [Module]) ->
    Remote = kast_app_deps:remote_calls_from_module(list_to_atom(Module)),
    io:format("remote module calls: ~p~n", [lists:sort(Remote)]);
handle(['circle'], []) ->
    Circles = kast_app_deps:cicles(),
    print_circles(Circles);
handle(['circle'], [App]) ->
    {A, Circles} = kast_app_deps:circles(list_to_atom(App)),
    print_circles([{A, Circles}]);
handle(_, _) ->
    print_help().

list_remote_apps(App) ->
    Apps =  kast_app_deps:remote_apps(list_to_atom(App)),
    io:format("~n~p~n", [lists:keysort(2, Apps)]).

print_circles(Circles) ->
    [print_circle(App, Deps)
     || {App, Deps} <- Circles,
        Deps =/= []
    ],
    'ok'.

print_circle(App, Deps) ->
    io:format("app ~s has circular dep with ~s~n"
             ,[App, kz_binary:join(Deps, <<", ">>)]
             ).


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
    ,{'module', $m, "module", 'undefined', "The module to process"}
    ,{'app', $a, "app", 'undefined', "The app to process"}
    ,{'project', $p, "project", 'undefined', "Process the project"}
    ,{'dot', $d, "dot", 'undefined', "Generate a DOT file"}
    ,{'circle', $c, "circle", 'undefined', "Calculate circle dependencies"}
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
    getopt:usage(option_spec_list(), "ERL_LIBS=deps/:core/:applications/ ./scripts/apps_of_app.escript", "[args ...]"),
    halt(1).
