%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(gql_core_scalar).

-export([input/2
        ,output/2
        ]).

-include("kz_graphql.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec input(any(), any()) -> any().
input(<<"CallRecordingTimeLimit">>, Input) ->
    case kz_term:safe_cast(fun kz_term:to_integer/1, Input, 'undefined') of
        I when is_integer(I)
               andalso I >= 5
               andalso I =< 10800 ->
            {'ok', I};
        _ ->
            {'error', <<"Value must be bigger than 5 and lower than 10800">>}
    end;
input(<<"DateTime">>, Input) ->
    %% TODO: use iso8601 https://github.com/erlsci/iso8601
    %X FIXME: This probably is going to crash due to a bug in the library
    try rfc3339:parse(Input) of
        {'ok', {Date, Time, _USec, _TZ}} ->
            {'ok', calendar:datetime_to_gregorian_seconds({Date, Time})}
    catch
        _E:_T ->
            {'error', 'bad_date'}
    end;
input(<<"Email">>, Input) ->
    case re:run(Input, <<"^[^@]+@[^@]+$">>, [{'capture', 'none'}, 'unicode']) of
        'match' -> {'ok', Input};
        'nomatch' -> {'error', <<"Bad Email address">>}
    end;
input(<<"RequestFormattersKey">>, Input) ->
    case re:run(Input, <<"^[[:alnum:]_]$">>, [{'capture', 'none'}, 'unicode']) of
        'match' -> {'ok', Input};
        'nomatch' -> {'error', <<"Bad route request key name">>}
    end;
input(<<"Regex">>, Input) ->
    case re:compile(Input) of
        {'ok', _} -> {'ok', Input};
        {'error', _} -> {'error', <<"Bad regex string">>}
    end;
input(<<"String35">>, Input) ->
    case size(Input) < 36 of
        'true' -> {'ok', Input};
        'false' -> {'error', <<"String35 value can not ne more than 35 characters">>}
    end;
input(<<"URI">>, Input) ->
    try kz_http_util:urlsplit(Input) of
        _ -> {'ok', Input}
    catch
        _T:_E ->
            {'error', <<"Value is not a valid URI">>}
    end.

-spec output(any(), any()) -> any().
output(<<"DateTime">>, {Date, Time}) ->
    %% TODO: use iso8601 https://github.com/erlsci/iso8601
    %X FIXME: This probably is going to crash due to a bug in the library
    {'ok', rfc3339:format({Date, Time, 0, 0})};
output(_Type, Val) ->
    {'ok', Val}.
