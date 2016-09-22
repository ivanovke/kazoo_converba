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
handle(_, _) ->
    print_help().

list_remote_apps(App) ->
    Apps =  kast_app_deps:remote_apps(list_to_atom(App)),
    io:format("~n~p~n", [lists:keysort(2, Apps)]).


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
    getopt:usage(option_spec_list(), "ERL_LIBS=deps/:core/:applications/ ./scripts/apps_of_app.escript", "[args ...]"),
    halt(1).
