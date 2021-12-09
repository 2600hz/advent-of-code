#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 9: Smoke Basin ---

%% These caves seem to be lava tubes. Parts are even still
%% volcanically active; small hydrothermal vents release smoke into
%% the caves that slowly settles like rain.

%% If you can model how the smoke flows through the caves, you might
%% be able to avoid it and be that much safer. The submarine generates
%% a heightmap of the floor of the nearby caves for you (your puzzle
%% input).

%% Smoke flows to the lowest point of the area it's in. For example,
%% consider the following heightmap:

%% 2199943210
%% 3987894921
%% 9856789892
%% 8767896789
%% 9899965678

%% Each number corresponds to the height of a particular location,
%% where 9 is the highest and 0 is the lowest a location can be.

%% Your first goal is to find the low points - the locations that are
%% lower than any of its adjacent locations. Most locations have four
%% adjacent locations (up, down, left, and right); locations on the
%% edge or corner of the map have three or two adjacent locations,
%% respectively. (Diagonal locations do not count as adjacent.)

%% In the above example, there are four low points, all highlighted:
%% two are in the first row (a 1 and a 0), one is in the third row (a
%% 5), and one is in the bottom row (also a 5). All other locations on
%% the heightmap have some lower adjacent location, and so are not low
%% points.

%% The risk level of a low point is 1 plus its height. In the above
%% example, the risk levels of the low points are 2, 1, 6, and 6. The
%% sum of the risk levels of all low points in the heightmap is
%% therefore 15.

%% Find all of the low points on your heightmap. What is the sum of
%% the risk levels of all low points on your heightmap?

main(_) ->
    Input = read_input("p9.txt"),
    p9_1(Input),
    p9_2(Input).

p9_1(HeightMap) ->
    LowPoints = low_points(HeightMap),
    Risk = lists:sum([1 + Point || Point <- LowPoints]),
    io:format("risk: ~p~n", [Risk]).

low_points(HeightMap) ->
    LowPoints = maps:fold(fun is_low_point/3, HeightMap, HeightMap),
    [N || N <- maps:values(LowPoints), is_integer(N)].

is_low_point({X, Y}, Height, HeightMap) ->
    case maps:get({X, Y}, HeightMap, 'undefined') of
        'undefined' ->
            %% io:format("point {~p, ~p} removed~n", [X, Y]),
            %% already cleared as a not-low-point
            HeightMap;
        {'high', _} -> HeightMap;
        Height ->
            SurroundingPoints = [{{X+PX, Y+PY}, maps:get({X+PX, Y+PY}, HeightMap, 10)}
                                 || PX <- [-1, 0, 1],
                                    PY <- [-1, 0, 1],
                                    {X, Y} =/= {X+PX, Y+PY}
                                ],
            case lists:partition(fun({_, H}) -> not is_higher(Height, H) end, SurroundingPoints) of
                {[], HigherPoints} ->
                    %% io:format("point ~p(~p) has hp: ~w~n", [{X, Y}, Height, HigherPoints]),
                    lists:foldl(fun add_higher/2, HeightMap, HigherPoints);
                {_LowerPoints, HigherPoints} ->
                    %% io:format("point ~p(~p) is not lowest: ~p hp: ~w~n", [{X, Y}, Height, LowerPoints, HigherPoints]),
                    lists:foldl(fun add_higher/2, HeightMap, [{{X, Y}, Height} | HigherPoints])
            end
    end.

is_higher(Height, {'high', N}) ->
    Height < N;
is_higher(Height, N) ->
    Height < N.

add_higher({{X, Y}, _H}, HeightMap) ->
    case maps:get({X, Y}, HeightMap, 'undefined') of
        'undefined' -> HeightMap;
        {'high', _} -> HeightMap;
        N -> maps:put({X, Y}, {'high', N}, HeightMap)
    end.

p9_2(Input) ->
    Input.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    {_X, _Y, HeightMap} = lists:foldl(fun parse_line/2
                                     ,{0,0,#{}}
                                     ,binary:split(Lines, <<"\n">>, ['global'])
                                     ),
    HeightMap.

parse_line(<<>>, Acc) -> Acc;
parse_line(Line, Acc) ->
    {_X, Y, HeightMap} = lists:foldl(fun add_height/2, Acc, binary_to_list(Line)),
    {0, Y+1, HeightMap}.

add_height(NBin, {X, Y, HeightMap}) ->
    {X+1, Y, maps:put({X, Y}, NBin - $0, HeightMap)}.
