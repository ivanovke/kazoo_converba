%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(gql_core_query).

-export([execute/4
        ]).

-include("raptor.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec execute(any(), any(), any(), any()) -> any().
execute(Context, Object, Field, Args) ->
    %% authenticate
    run_execute(Context, Object, Field, Args).

-spec run_execute(any(), any(), any(), any()) -> any().
run_execute(Context, _Object, <<"node">>, #{<<"id">> := ID}) ->
    load_node(Context, 'any', ID);
run_execute(Context, _Object, <<"account">>, #{<<"id">> := ID}) ->
    load_doc(Context, <<"account">>, kz_util:format_account_id(ID)).

load_node(Context, 'any', InputID) ->
    try
        case binary:split(InputID, <<":">>) of
            [Type, ID] ->
                load_doc(Context, 'any', {Type, ID});
            _Error ->
                ?DEV_LOG("~p psplit error: ~p", [InputID, _Error]),
                exit('invalid_id')
        end
    catch
        _E:_T ->
            ?DEV_LOG("~p try error ~p:~p", [InputID, _E, _T]),
            {'error', 'invalid_id'}
    end.

load_doc(Context, 'any', {Type, ID}) ->
    load_doc(Context, [Type], ID);
load_doc(Context, Types, {Type, ID}) ->
    case lists:member(Type, Types) of
        'true' -> load_doc(Context, Type, ID);
        'false' ->
            ?DEV_LOG("type ~p is not member of ~p", [Type, Types]),
            {'error', 'invalid_id'}
    end;
load_doc(_Context, Type, ID) ->
    %% dummy load for now
    case kz_datamgr:open_doc(kz_util:format_account_db(ID), ID) of
        {'ok', JObj} ->
            case kz_doc:type(JObj) of
                Type -> {'ok', #{'$type' => Type
                                ,'object' => kz_json:to_map(JObj)
                                }
                        };
                _Other ->
                    ?DEV_LOG("expected type ~p is not document ~p with ~p", [Type, ID, _Other]),
                    {'error', 'invalid_id'}
            end;
        {'error', _}=Error -> Error
    end.

