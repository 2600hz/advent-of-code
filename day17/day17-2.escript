#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% For some reason, your simulated results don't match what the
%% experimental energy source engineers expected. Apparently, the
%% pocket dimension actually has four spatial dimensions, not three.

%% The pocket dimension contains an infinite 4-dimensional grid. At
%% every integer 4-dimensional coordinate (x,y,z,w), there exists a
%% single cube (really, a hypercube) which is still either active or
%% inactive.

%% Each cube only ever considers its neighbors: any of the 80 other
%% cubes where any of their coordinates differ by at most 1. For
%% example, given the cube at x=1,y=2,z=3,w=4, its neighbors include
%% the cube at x=2,y=2,z=3,w=3, the cube at x=0,y=2,z=3,w=4, and so on.

%% The initial state of the pocket dimension still consists of a small
%% flat region of cubes. Furthermore, the same rules for cycle updating
%% still apply: during each cycle, consider the number of active
%% neighbors of each cube.

%% For example, consider the same initial state as in the example
%% above. Even though the pocket dimension is 4-dimensional, this
%% initial state represents a small 2-dimensional slice of it. (In
%% particular, this initial state defines a 3x3x1x1 region of the
%% 4-dimensional space.)

%% Simulating a few cycles from this initial state produces the
%% following configurations, where the result of each cycle is shown
%% layer-by-layer at each given z and w coordinate:

%% Before any cycles:

%% z=0, w=0
%% .#.
%% ..#
%% ###


%% After 1 cycle:

%% z=-1, w=-1
%% #..
%% ..#
%% .#.

%% z=0, w=-1
%% #..
%% ..#
%% .#.

%% z=1, w=-1
%% #..
%% ..#
%% .#.

%% z=-1, w=0
%% #..
%% ..#
%% .#.

%% z=0, w=0
%% #.#
%% .##
%% .#.

%% z=1, w=0
%% #..
%% ..#
%% .#.

%% z=-1, w=1
%% #..
%% ..#
%% .#.

%% z=0, w=1
%% #..
%% ..#
%% .#.

%% z=1, w=1
%% #..
%% ..#
%% .#.


%% After 2 cycles:

%% z=-2, w=-2
%% .....
%% .....
%% ..#..
%% .....
%% .....

%% z=-1, w=-2
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=0, w=-2
%% ###..
%% ##.##
%% #...#
%% .#..#
%% .###.

%% z=1, w=-2
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=2, w=-2
%% .....
%% .....
%% ..#..
%% .....
%% .....

%% z=-2, w=-1
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=-1, w=-1
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=0, w=-1
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=1, w=-1
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=2, w=-1
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=-2, w=0
%% ###..
%% ##.##
%% #...#
%% .#..#
%% .###.

%% z=-1, w=0
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=0, w=0
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=1, w=0
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=2, w=0
%% ###..
%% ##.##
%% #...#
%% .#..#
%% .###.

%% z=-2, w=1
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=-1, w=1
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=0, w=1
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=1, w=1
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=2, w=1
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=-2, w=2
%% .....
%% .....
%% ..#..
%% .....
%% .....

%% z=-1, w=2
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=0, w=2
%% ###..
%% ##.##
%% #...#
%% .#..#
%% .###.

%% z=1, w=2
%% .....
%% .....
%% .....
%% .....
%% .....

%% z=2, w=2
%% .....
%% .....
%% ..#..
%% .....
%% .....

%% After the full six-cycle boot process completes, 848 cubes are left
%% in the active state.

%% Starting with your given initial configuration, simulate six cycles
%% in a 4-dimensional space. How many cubes are left in the active
%% state after the sixth cycle?

-define(ACTIVE, $#).
-define(INACTIVE, $.).

main(_) ->
    {_, Active} = read_input("p17.txt"),
    Six = lists:foldl(fun(_N, A) ->
                              Acc = run_cycle(A),
                              io:format("~p: ~p~n", [_N, length(Acc)]),
                              Acc
                      end
                     ,Active
                     ,lists:seq(1, 6)
                     ),
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
    PointIsActive = lists:member(Point, Active),

    case lists:foldl(fun count_neighbors/2
                    ,{Point, 0}
                    ,Active
                    )
    of
        {Point, 3} when not PointIsActive ->
            %% io:format("activate ~p~n", [Point]),
            [Point | Acc];
        {Point, 2} when PointIsActive ->
            %% io:format("remain ~p~n", [Point]),
            [Point | Acc];
        {Point, 3} when PointIsActive ->
            %% io:format("remain ~p~n", [Point]),
            [Point | Acc];
        _ ->
            Acc
    end.


count_neighbors(XYZW, {XYZW, Count}) ->
    %% don't count ourselves
    {XYZW, Count};
count_neighbors({Ax, Ay, Az, Aw}, {{Px, Py, Pz, Pw}=Point, Count}) ->
    case (abs(Ax-Px) =< 1)
        andalso (abs(Ay-Py) =< 1)
        andalso (abs(Az-Pz) =< 1)
        andalso (abs(Aw-Pw) =< 1)
    of
        'true' -> {Point, Count+1};
        'false' -> {Point, Count}
    end.

add_point({X, Y, Z, W}, Points) ->
    [{Dx, Dy, Dz, Dw} || Dx <- [X-1, X, X+1],
                        Dy <- [Y-1, Y, Y+1],
                        Dz <- [Z-1, Z, Z+1],
                        Dw <- [W-1, W, W+1]
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
    {{X+1, Y}, [{X, Y, 0, 0} | Active]};
add_coordinate(?INACTIVE, {{X, Y}, Active}) ->
    {{X+1, Y}, Active}.
