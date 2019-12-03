#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Day 3: Crossed Wires ---

%% The gravity assist was successful, and you're well on your way to
%% the Venus refuelling station. During the rush back on Earth, the
%% fuel management system wasn't completely installed, so that's next
%% on the priority list.

%% Opening the front panel reveals a jumble of wires. Specifically,
%% two wires are connected to a central port and extend outward on a
%% grid. You trace the path each wire takes as it leaves the central
%% port, one wire per line of text (your puzzle input).

%% The wires twist and turn, but the two wires occasionally cross
%% paths. To fix the circuit, you need to find the intersection point
%% closest to the central port. Because the wires are on a grid, use
%% the Manhattan distance for this measurement. While the wires do
%% technically cross right at the central port where they both start,
%% this point does not count, nor does a wire count as crossing with
%% itself.

%% For example, if the first wire's path is R8,U5,L5,D3, then starting
%% from the central port (o), it goes right 8, up 5, left 5, and
%% finally down 3:

%% ...........
%% ...........
%% ...........
%% ....+----+.
%% ....|....|.
%% ....|....|.
%% ....|....|.
%% .........|.
%% .o-------+.
%% ...........

%% Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right
%% 6, down 4, and left 4:

%% ...........
%% .+-----+...
%% .|.....|...
%% .|..+--X-+.
%% .|..|..|.|.
%% .|.-X--+.|.
%% .|..|....|.
%% .|.......|.
%% .o-------+.
%% ...........

%% These wires cross at two locations (marked X), but the lower-left
%% one is closer to the central port: its distance is 3 + 3 = 6.

%% Here are a few more examples:

%%     R75,D30,R83,U83,L12,D49,R71,U7,L72
%%     U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
%%     R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
%%     U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135

%% What is the Manhattan distance from the central port to the closest
%% intersection?

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    {Wire1, Wire2} = read_wire_paths(),
    %% io:format("wires:~n~p~n ~p~n", [Wire1, Wire2]),
    Wire1Points = wire_path_to_points(Wire1),
    Wire2Points = wire_path_to_points(Wire2),
    Intersection = sets:intersection(Wire1Points, Wire2Points),
    IntersectionPoints = sets:to_list(Intersection),
    %% io:format("intersections at ~p~n", [IntersectionPoints]),
    {X, Y, Distance} = find_closest_intersection(IntersectionPoints),
    io:format("closest is {~p, ~p} = ~p~n", [X, Y, Distance]).

find_closest_intersection([{X, Y} | IntersectionPoints]) ->
    Distance = aoc:manhattan_distance({0, 0}, {X, Y}),
    find_closest_intersection(IntersectionPoints, {X, Y, Distance}).

find_closest_intersection([], Closest) -> Closest;
find_closest_intersection([{X, Y} | IntersectionPoints], {_CX, _CY, CD}=Closest) ->
    case aoc:manhattan_distance({0, 0}, {X, Y}) of
        Distance when Distance < CD ->
            find_closest_intersection(IntersectionPoints, {X, Y, Distance});
        _Distance ->
            find_closest_intersection(IntersectionPoints, Closest)
    end.

wire_path_to_points(Wire) ->
    Origin = {0,0},
    wire_path_to_points(Wire, Origin, []).

wire_path_to_points([], _CurrentPoint, Points) -> sets:from_list(Points);
wire_path_to_points([{Direction, Distance} | WirePath], CurrentPoint, Points) ->
    {NewPoint, MorePoints} = add_points(Direction, Distance, CurrentPoint, Points),
    wire_path_to_points(WirePath, NewPoint, MorePoints).

add_points(_Direction, 0, CurrentPoint, Points) ->
    {CurrentPoint, Points};
add_points(Direction, Distance, CurrentPoint, Points) ->
    NextPoint = next_point(Direction, CurrentPoint),
    add_points(Direction, Distance-1, NextPoint, [NextPoint | Points]).

next_point('right', {X, Y}) -> {X+1, Y};
next_point('left', {X, Y}) ->  {X-1, Y};
next_point('up', {X, Y}) ->    {X, Y+1};
next_point('down', {X, Y}) ->  {X, Y-1}.

read_wire_paths() ->
    Contents = read_input(),
    binary_to_wire_paths(Contents).

binary_to_wire_paths(Contents) ->
    [Wire1, Wire2] = binary_to_wire_paths(Contents, []),
    {Wire1, Wire2}.

binary_to_wire_paths(<<"\n", Rest/binary>>, Paths) ->
    binary_to_wire_paths(Rest, Paths);
binary_to_wire_paths(<<>>, Paths) ->
    lists:reverse(Paths);
binary_to_wire_paths(Contents, Paths) ->
    {Path, Rest} = binary_to_wire_path(Contents),
    binary_to_wire_paths(Rest, [Path | Paths]).

binary_to_wire_path(Contents) ->
    [PathBin, Rest] = binary:split(Contents, <<"\n">>),
    Directions = binary:split(PathBin, <<",">>, ['global']),
    {[{binary_to_direction(Direction), binary_to_integer(Amount)}
      || <<Direction:1/binary, Amount/binary>> <- Directions
     ]
    ,Rest
    }.

binary_to_direction(<<"R">>) -> 'right';
binary_to_direction(<<"L">>) -> 'left';
binary_to_direction(<<"U">>) -> 'up';
binary_to_direction(<<"D">>) -> 'down'.

read_input() ->
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.

%% test_input() ->
%% <<"R8,U5,L5,D3\n"
%%   "U7,R6,D4,L4\n"
%% >>. %% distance: 6
%% <<"R75,D30,R83,U83,L12,D49,R71,U7,L72\n"
%%   "U62,R66,U55,R34,D71,R55,D58,R83\n"
%% >>. %% distance: 159
%% <<"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\n"
%%   "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7\n"
%% >>. %% distance: 135
