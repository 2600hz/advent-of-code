#!/usr/bin/env escript
%%! +A0 -sname day1
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([]) ->
    {ok, Input} = file:read_file("puzzle.txt"),
    %% {ok, Input} = file:read_file("sample.txt"),

    Lines = binary:split(Input, <<"\n">>, ['global', 'trim']),

    {Left, Right} = split_lists(Lines),

    Distances = calc_distances(Left, Right),
    Total = lists:sum(Distances),
    io:format("part 1: ~p~n", [Total]),

    Score = similarity_score(Left, Right),
    io:format("part 2: ~p~n", [Score]).

similarity_score(Left, Right) ->
    similarity_score(Left, Right, #{}, 0).

similarity_score([], _Right, _Map, Score) ->
    Score;
similarity_score([L | Left], Right, Map, Score) ->
    LScore =
        case maps:get(L, Map, 'undefined') of
            'undefined' -> lists:sum([1 || R <- Right, R =:= L]) * L;
            S -> S
        end,
    similarity_score(Left, Right, Map#{L => LScore}, LScore + Score).

split_lists(Lines) ->
    split_lists(Lines, [], []).

split_lists([], Left, Right) ->
    {lists:sort(Left), lists:sort(Right)};
split_lists([Row | Lines], Left, Right) ->
    [LeftBin, RightBin] = [Num || Num <- binary:split(Row, <<" ">>, ['global', 'trim']),
                                  Num =/= <<>>
                          ],
    split_lists(Lines, [binary_to_integer(LeftBin) | Left], [binary_to_integer(RightBin) | Right]).

calc_distances(Left, Right) ->
    calc_distances(Left, Right, []).

calc_distances([], [], Distances) ->
    lists:reverse(Distances);
calc_distances([LNum | Left], [RNum | Right], Distances) ->
    calc_distances(Left, Right, [abs(LNum - RNum) | Distances]).
