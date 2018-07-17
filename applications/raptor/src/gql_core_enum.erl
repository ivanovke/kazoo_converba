%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(gql_core_enum).

-export([input/2
        ,output/2
        ]).

-include("raptor.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec input(any(), any()) -> any().
input(<<"CallRestrictionEnum">>, <<"INHERIT">>=Val) -> {'ok', Val};
input(<<"CallRestrictionEnum">>, <<"DENY">>=Val) -> {'ok', Val};
input(<<"AudioRecordingFormat">>, <<"MP3">>=Val) -> {'ok', Val};
input(<<"AudioRecordingFormat">>, <<"WAV">>=Val) -> {'ok', Val}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec output(any(), any()) -> any().
output(<<"CallRestrictionEnum">>, Val) -> {'ok', Val};
output(<<"AudioRecordingFormat">>, Val) -> {'ok', Val}.
