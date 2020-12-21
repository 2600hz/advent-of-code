#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 17: Conway Cubes ---

%% As your flight slowly drifts through the sky, the Elves at the
%% Mythical Information Bureau at the North Pole contact you. They'd
%% like some help debugging a malfunctioning experimental energy source
%% aboard one of their super-secret imaging satellites.

%% The experimental energy source is based on cutting-edge technology:
%% a set of Conway Cubes contained in a pocket dimension! When you hear
%% it's having problems, you can't help but agree to take a look.

%% The pocket dimension contains an infinite 3-dimensional grid. At
%% every integer 3-dimensional coordinate (x,y,z), there exists a
%% single cube which is either active or inactive.

%% In the initial state of the pocket dimension, almost all cubes start
%% inactive. The only exception to this is a small flat region of cubes
%% (your puzzle input); the cubes in this region start in the specified
%% active (#) or inactive (.) state.

%% The energy source then proceeds to boot up by executing six cycles.

%% Each cube only ever considers its neighbors: any of the 26 other
%% cubes where any of their coordinates differ by at most 1. For
%% example, given the cube at x=1,y=2,z=3, its neighbors include the
%% cube at x=2,y=2,z=2, the cube at x=0,y=2,z=3, and so on.

%% During a cycle, all cubes simultaneously change their state
%% according to the following rules:

%%     If a cube is active and exactly 2 or 3 of its neighbors are also
%%     active, the cube remains active. Otherwise, the cube becomes
%%     inactive.
%%     If a cube is inactive but exactly 3 of its neighbors are active,
%%     the cube becomes active. Otherwise, the cube remains inactive.

%% The engineers responsible for this experimental energy source would
%% like you to simulate the pocket dimension and determine what the
%% configuration of cubes should be at the end of the six-cycle boot
%% process.

%% For example, consider the following initial state:

%% .#.
%% ..#
%% ###

%% Even though the pocket dimension is 3-dimensional, this initial
%% state represents a small 2-dimensional slice of it. (In particular,
%% this initial state defines a 3x3x1 region of the 3-dimensional
%% space.)

%% Simulating a few cycles from this initial state produces the
%% following configurations, where the result of each cycle is shown
%% layer-by-layer at each given z coordinate (and the frame of view
%% follows the active cells in each cycle):

%% Before any cycles:

%% z=0
%% .#.
%% ..#
%% ###

%% After 1 cycle:

%% z=-1
%% #..
%% ..#
%% .#.

%% z=0
%% #.#
%% .##
%% .#.

%% z=1
%% #..
%% ..#
%% .#.

%% After 2 cycles:

%% z=-2
%% .....
%% .....
%% ..#..
%% .....
%% .....

%% z=-1
%% ..#..
%% .#..#
%% ....#
%% .#...
%% .....

%% z=0
%% ##...
%% ##...
%% #....
%% ....#
%% .###.

%% z=1
%% ..#..
%% .#..#
%% ....#
%% .#...
%% .....

%% z=2
%% .....
%% .....
%% ..#..
%% .....
%% .....

%% After 3 cycles:

%% z=-2
%% .......
%% .......
%% ..##...
%% ..###..
%% .......
%% .......
%% .......

%% z=-1
%% ..#....
%% ...#...
%% #......
%% .....##
%% .#...#.
%% ..#.#..
%% ...#...

%% z=0
%% ...#...
%% .......
%% #......
%% .......
%% .....##
%% .##.#..
%% ...#...

%% z=1
%% ..#....
%% ...#...
%% #......
%% .....##
%% .#...#.
%% ..#.#..
%% ...#...

%% z=2
%% .......
%% .......
%% ..##...
%% ..###..
%% .......
%% .......
%% .......

%% After the full six-cycle boot process completes, 112 cubes are left
%% in the active state.

%% Starting with your given initial configuration, simulate six
%% cycles. How many cubes are left in the active state after the sixth
%% cycle?

-define(ACTIVE, $#).
-define(INACTIVE, $.).

main(_) ->
    {_, Active} = read_input("p17.txt"),
    Six = lists:foldl(fun(_, A) -> run_cycle(A) end, Active, lists:seq(1, 6)),
    io:format("active: ~p~n", [length(Six)]).

run_cycle(Active) ->
    Points = lists:usort(lists:foldl(fun add_point/2, [], Active)),
    run_cycle(Active, Points).

run_cycle(Active, Points) ->
    lists:foldl(fun(P, Acc) -> is_active_point(P, Acc, Active) end
               ,[]
               ,Points
               ).

is_active_point(Point, Acc, Active) ->
    case lists:foldl(fun count_neighbors/2
                    ,{Point, ?INACTIVE, 0}
                    ,Active
                    )
    of
        {Point, ?INACTIVE, 3} ->
            %% io:format("activate ~p~n", [Point]),
            [Point | Acc];
        {Point, ?ACTIVE, 2} ->
            %% io:format("remain ~p~n", [Point]),
            [Point | Acc];
        {Point, ?ACTIVE, 3} ->
            %% io:format("remain ~p~n", [Point]),
            [Point | Acc];
        _ ->
            Acc
    end.


count_neighbors(XYZ, {XYZ, _State, Count}) ->
    %% don't count ourselves
    {XYZ, ?ACTIVE, Count};
count_neighbors({Ax, Ay, Az}, {{Px, Py, Pz}=Point, State, Count}) ->
    Distance = math:sqrt(math:pow((Ax-Px), 2) +
                             math:pow((Ay-Py), 2) +
                             math:pow((Az-Pz), 2)
                        ),
    case Distance < 2.0 of
        'true' -> {Point, State, Count+1};
        'false' -> {Point, State, Count}
    end.

add_point({X, Y, Z}, Points) ->
    [{Dx, Dy, Dz} || Dx <- [X-1, X, X+1],
                    Dy <- [Y-1, Y, Y+1],
                    Dz <- [Z-1, Z, Z+1]
    ] ++ Points.

read_input(Filename) ->
    {'ok', File} = file:read_file(Filename),
    lists:foldl(fun parse_line/2
               ,{{0, 0}, []}
               ,binary:split(File, <<"\n">>, ['global', 'trim'])
               ).

parse_line(Line, {{X, Y}, Active}) ->
    {{_X, _Y}, MoreActive} =
        lists:foldl(fun add_coordinate/2
                   ,{{X, Y}, Active}
                   ,binary_to_list(Line)
                   ),
    {{0, Y+1}, MoreActive}.

add_coordinate(?ACTIVE, {{X, Y}, Active}) ->
    {{X+1, Y}, [{X, Y, 0} | Active]};
add_coordinate(?INACTIVE, {{X, Y}, Active}) ->
    {{X+1, Y}, Active}.
