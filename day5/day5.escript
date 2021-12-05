#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 5: Hydrothermal Venture ---

%% You come across a field of hydrothermal vents on the ocean floor!
%% These vents constantly produce large, opaque clouds, so it would be
%% best to avoid them if possible.

%% They tend to form in lines; the submarine helpfully produces a list
%% of nearby lines of vents (your puzzle input) for you to review. For
%% example:

%% 0,9 -> 5,9
%% 8,0 -> 0,8
%% 9,4 -> 3,4
%% 2,2 -> 2,1
%% 7,0 -> 7,4
%% 6,4 -> 2,0
%% 0,9 -> 2,9
%% 3,4 -> 1,4
%% 0,0 -> 8,8
%% 5,5 -> 8,2

%% Each line of vents is given as a line segment in the format x1,y1
%% -> x2,y2 where x1,y1 are the coordinates of one end the line
%% segment and x2,y2 are the coordinates of the other end. These line
%% segments include the points at both ends. In other words:

%%     An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
%%     An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

%% For now, only consider horizontal and vertical lines: lines where
%% either x1 = x2 or y1 = y2.

%% So, the horizontal and vertical lines from the above list would
%% produce the following diagram:

%% .......1..
%% ..1....1..
%% ..1....1..
%% .......1..
%% .112111211
%% ..........
%% ..........
%% ..........
%% ..........
%% 222111....

%% In this diagram, the top left corner is 0,0 and the bottom right
%% corner is 9,9. Each position is shown as the number of lines which
%% cover that point or . if no line covers that point. The top-left
%% pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row
%% is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

%% To avoid the most dangerous areas, you need to determine the number
%% of points where at least two lines overlap. In the above example,
%% this is anywhere in the diagram with a 2 or larger - a total of 5
%% points.

%% Consider only horizontal and vertical lines. At how many points do
%% at least two lines overlap?

main(_) ->
    Input = read_input("p5.txt"),
    p5_1(Input),
    p5_2(Input).

p5_1(Vents) ->
    HorizVert = [Vent
                 || {{X1,Y1}, {X2,Y2}}=Vent <- Vents,
                    X1=:=X2 orelse Y1=:=Y2
                ],
    Intersections = map_points(HorizVert),
    Overlaps = maps:fold(fun count_multiple_intersections/3, 0, Intersections),
    io:format("overlaps: ~p multiple overlaps: ~p~n", [maps:size(Intersections), Overlaps]).

count_multiple_intersections(_P, 1, Count) -> Count;
count_multiple_intersections(_P, _N, Count) -> Count+1.

map_points(Lines) ->
    lists:foldl(fun map_point/2, #{}, Lines).

map_point({{X, Y1}, {X, Y2}}, Acc) ->
    add_points({x, X}, lists:sort([Y1, Y2]), Acc);
map_point({{X1, Y}, {X2, Y}}, Acc) ->
    add_points({y, Y}, lists:sort([X1, X2]), Acc).

add_points(P, [Min, Max], Acc) ->
    {P, Acc1} = lists:foldl(fun add_point/2, {P, Acc}, lists:seq(Min, Max)),
    Acc1.

add_point(P1, {P, Intersections}) ->
    {P, maps:update_with(make_point(P, P1), fun(V) -> V+1 end, 1, Intersections)}.

make_point({x, X}, Y) -> {X, Y};
make_point({y, Y}, X) -> {X, Y}.

p5_2(Input) ->
    Input.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [parse_vent_coordinates(Line)
     || Line <- binary:split(Lines, <<"\n">>, ['global']),
        Line =/= <<>>
    ].

%% Line = x1,y1 -> x2,y2
parse_vent_coordinates(Line) ->
    {'match', [[X1], [Y1], [X2], [Y2]]} =
        re:run(Line
              ,<<"(\\d+)">>
              ,[{capture, all_but_first, binary}, global]
              ),
    {{binary_to_integer(X1, 10), binary_to_integer(Y1, 10)}
    ,{binary_to_integer(X2, 10), binary_to_integer(Y2, 10)}
    }.
