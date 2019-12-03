#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% It turns out that this circuit is very timing-sensitive; you
%% actually need to minimize the signal delay.

%% To do this, calculate the number of steps each wire takes to reach
%% each intersection; choose the intersection where the sum of both
%% wires' steps is lowest. If a wire visits a position on the grid
%% multiple times, use the steps value from the first time it visits
%% that position when calculating the total value of a specific
%% intersection.

%% The number of steps a wire takes is the total number of grid
%% squares the wire has entered to get to that location, including the
%% intersection being considered. Again consider the example from
%% above:

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

%% In the above example, the intersection closest to the central port
%% is reached after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 =
%% 20 steps by the second wire for a total of 20+20 = 40 steps.

%% However, the top-right intersection is better: the first wire takes
%% only 8+5+2 = 15 and the second wire takes only 7+6+2 = 15, a total
%% of 15+15 = 30 steps.

%% Here are the best steps for the extra examples from above:

%%     R75,D30,R83,U83,L12,D49,R71,U7,L72
%%     U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
%%     R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
%%     U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps

%% What is the fewest combined steps the wires must take to reach an
%% intersection?

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    {Wire1, Wire2} = read_wire_paths(),
    Wire1Points = wire_path_to_points(Wire1),
    Wire2Points = wire_path_to_points(Wire2),

    Intersection = sets:intersection(sets:from_list(Wire1Points), sets:from_list(Wire2Points)),
    IntersectionPoints = sets:to_list(Intersection),
    {_W1, _W2, Delay} = find_shortest_delay(Wire1Points
                                           ,Wire2Points
                                           ,IntersectionPoints
                                           ),
    io:format("shortest delay: ~p~n", [Delay]).

find_shortest_delay(Wire1Points, Wire2Points, [{X, Y} |IntersectionPoints]) ->
    Wire1Delay = signal_delay(Wire1Points, {X, Y}),
    Wire2Delay = signal_delay(Wire2Points, {X, Y}),
    find_shortest_delay(Wire1Points, Wire2Points, IntersectionPoints, {Wire1Delay, Wire2Delay, Wire1Delay + Wire2Delay}).

find_shortest_delay(_Wire1Points, _Wire2Points, [], CurrentDelay) -> CurrentDelay;
find_shortest_delay(Wire1Points, Wire2Points, [{X, Y} | IntersectionPoints], {DX, DY, DD}) ->
    Wire1Delay = signal_delay(Wire1Points, {X, Y}),
    Wire2Delay = signal_delay(Wire2Points, {X, Y}),
    case Wire1Delay + Wire2Delay of
        Shortest when Shortest < DD ->
            find_shortest_delay(Wire1Points, Wire2Points, IntersectionPoints, {Wire1Delay, Wire2Delay, Shortest});
        _Delay ->
            find_shortest_delay(Wire1Points, Wire2Points, IntersectionPoints, {DX, DY, DD})
    end.

signal_delay(WirePoints, {X,Y}) ->
    signal_delay(WirePoints, {X,Y}, 1).

signal_delay([Point | _], Point, Delay) -> Delay;
signal_delay([_P | Points], Point, Delay) ->
    signal_delay(Points, Point, Delay+1).

wire_path_to_points(Wire) ->
    Origin = {0,0},
    wire_path_to_points(Wire, Origin, []).

wire_path_to_points([], _CurrentPoint, Points) -> lists:reverse(Points);
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
%% >>. %% distance: 6, steps: 30
%% <<"R75,D30,R83,U83,L12,D49,R71,U7,L72\n"
%%   "U62,R66,U55,R34,D71,R55,D58,R83\n"
%% >>. %% distance: 159, steps: 610
%% <<"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\n"
%%   "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7\n"
%% >>. %% distance: 135, steps: 410
