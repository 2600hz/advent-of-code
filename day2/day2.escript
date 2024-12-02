#!/usr/bin/env escript
%%! +A0 -sname day1
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([]) ->
    {ok, Input} = file:read_file("puzzle.txt"),
    %% {ok, Input} = file:read_file("sample.txt"),
    Reports = [[binary_to_integer(Level)
                || Level <- binary:split(Report, <<" ">>, ['global'])
               ]
               || Report <- binary:split(Input, <<"\n">>, ['global', 'trim'])
              ],
    {SafeReports, _UnsafeReports} = lists:partition(fun is_safe_report/1, Reports),

    {DampSafe, _UnsafeDamp} = lists:partition(fun is_damp_report/1, Reports),

    io:format("safe: ~p~n", [length(SafeReports)]),
    io:format("damp: ~p~n", [length(DampSafe)]).

is_safe_report([L1, L2 | _]=Levels) ->
    is_safe_report(Levels, L1 < L2).

is_safe_report([], _Increasing) -> 'true';
is_safe_report([_], _Increasing) -> 'true';

%% increasing levels
is_safe_report([L1, L2 | Rest], 'true')
  when L1 < L2
       andalso L2-L1 < 4
       andalso L2-L1 > 0
       ->
    is_safe_report([L2 | Rest], 'true');

%% decreasing levels
is_safe_report([L1, L2 | Rest], 'false')
  when L1 > L2
       andalso L1-L2 < 4
       andalso L1-L2 > 0
       ->
    is_safe_report([L2 | Rest], 'false');
is_safe_report(_Rest, _Increasing) ->
    'false'.

%% too low: 381, 364, 367, 385, 375, 376
is_damp_report([L1, L2 | Rest] = Levels) ->
    is_damp_report(Levels, L1 < L2, [], 'false')
        orelse is_damp_report([L2 | Rest], L2 < hd(Rest), [], 'true').

is_damp_report([_], _, _, _) -> 'true';

%% asc and valid
is_damp_report([L1, L2 | Rest], 'true', ValidLevels, Restarted)
  when L1 < L2,
       L2-L1 > 0,
       L2-L1 < 4 ->
    is_damp_report([L2 | Rest], 'true', [L1 | ValidLevels], Restarted);

%% desc and valid
is_damp_report([L1, L2 | Rest], 'false', ValidLevels, Restarted)
  when L1 > L2,
       L1-L2 > 0,
       L1-L2 < 4 ->
    is_damp_report([L2 | Rest], 'false', [L1 | ValidLevels], Restarted);

%% invalid but not restarted
is_damp_report([L1, L2 | Rest], _IsAsc, ValidLevels, 'false') ->
    %% io:format("  invalid asc:~p: ~p -> ~p~n", [_IsAsc, L1, L2]),
    restart([L2 | Rest], ValidLevels)
        orelse restart([L1 | Rest], ValidLevels);
%% invalid and already restarted, unsafe
is_damp_report(_Levels, _, _, 'true') ->
    %% io:format("  not safe: ~p~n", [_Levels]),
    'false'.

restart(Levels, ValidLevels) ->
    [N1, N2 | _] = NewLevels = lists:reverse(ValidLevels) ++ Levels,
    %% io:format("  restarting with ~p~n", [NewLevels]),
    is_damp_report(NewLevels, N1 < N2, [], 'true').
