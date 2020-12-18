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
    {{X, Y, Z}, Space} = read_input("p17-test.txt"),
    InitAcc = {{-1,-1,-1}, {X+1, Y+1, Z+1}, {Space, Space}},
    {_, _, {_OldSpace, NewSpace}} =
        lists:foldl(fun(_C, Acc) -> run_cycle(Acc) end
                   ,InitAcc
                   ,lists:seq(1, 1)
                   ),
    io:format("active: ~p ~p~n", [active_cubes(Space)
                                 ,active_cubes(NewSpace)
                                 ]).

active_cubes(Space) ->
    maps:fold(fun active_cube/3, 0, Space).

active_cube(_XYZ, ?ACTIVE, Count) -> Count+1;
active_cube(_XYZ, ?INACTIVE, Count) -> Count.

run_cycle({{MinX, MinY, MinZ}
          ,{MaxX, MaxY, MaxZ}
          ,{_OldSpace, CurrentSpace}
          }
         ) ->
    Coordinates =
        [{X, Y, Z} || X <- lists:seq(MinX, MaxX),
                     Y <- lists:seq(MinY, MaxY),
                     Z <- lists:seq(MinZ, MaxZ)
        ],
    lists:foldl(fun run_cycle/2
               ,{{MinX, MinY, MinZ}
                ,{MaxX, MaxY, MaxZ}
                ,{CurrentSpace, #{}}
                }
               ,Coordinates
               ).

run_cycle({_, _, _}=XYZ, {Min, Max, {CurrentSpace, NewSpace}}) ->
    {update_min(XYZ, Min)
    ,update_max(XYZ, Max)
    ,{CurrentSpace, apply_rule(XYZ, CurrentSpace, NewSpace, maps:get(XYZ, CurrentSpace, ?INACTIVE))}
    }.

update_min({X, Y, Z}, {Mx, My, Mz}) ->
    {erlang:min(X, Mx), erlang:min(Y, My), erlang:min(Z, Mz)}.

update_max({X, Y, Z}, {Mx, My, Mz}) ->
    {erlang:max(X, Mx), erlang:max(Y, My), erlang:max(Z, Mz)}.

apply_rule(XYZ, CurrentSpace, NewSpace, ?ACTIVE) ->
    active_rule(XYZ, CurrentSpace, NewSpace);
apply_rule(XYZ, CurrentSpace, NewSpace, ?INACTIVE) ->
    inactive_rule(XYZ, CurrentSpace, NewSpace).

%% If a cube is active and exactly 2 or 3 of its neighbors are also
%% active, the cube remains active. Otherwise, the cube becomes
%% inactive.
active_rule({_, _, _}=XYZ, CurrentSpace, NewSpace) ->
    case active_neighbors(XYZ, CurrentSpace) of
        2 ->
            io:format("~p remains 2 active~n", [XYZ]),
            NewSpace#{XYZ => ?ACTIVE};
        3 ->
            io:format("~p remains 3 active~n", [XYZ]),
            NewSpace#{XYZ => ?ACTIVE};
        _N ->
            io:format("toggling ~p to inactive ~p~n", [XYZ, _N]),
            NewSpace#{XYZ => ?INACTIVE}
    end.

%% If a cube is inactive but exactly 3 of its neighbors are active, the
%% cube becomes active. Otherwise, the cube remains inactive.
inactive_rule({_, _, _}=XYZ, CurrentSpace, NewSpace) ->
    case active_neighbors(XYZ, CurrentSpace) of
        3 ->
            io:format("toggling ~p to active~n", [XYZ]),
            NewSpace#{XYZ => ?ACTIVE};
        _ ->
            io:format("~p remains inactive~n", [XYZ]),
            NewSpace#{XYZ => ?INACTIVE}
    end.

active_neighbors({X, Y, Z}, Space) ->
    lists:sum([1 || Dx <- [X-1, X, X+1],
                   Dy <- [Y-1, Y, Y+1],
                   Dz <- [Z-1, Z, Z+1],
                   {Dx, Dy, Dz} =/= {X, Y, Z},
                   ?ACTIVE =:= maps:get({Dx, Dy, Dx}, Space, ?INACTIVE)
              ]).

read_input(Filename) ->
    {'ok', File} = file:read_file(Filename),
    lists:foldl(fun parse_line/2
               ,{{0, 0, 0}, #{}}
               ,binary:split(File, <<"\n">>, ['global', 'trim'])
               ).

parse_line(Line, {{X, Y, Z}, Space}) ->
    {{_X, _Y, _Z}, NewSpace} =
        lists:foldl(fun add_coordinate/2
                   ,{{X, Y, Z}, Space}
                   ,binary_to_list(Line)
                   ),
    {{1, Y+1, Z}, NewSpace}.

add_coordinate(Char, {{X, Y, Z}, Space}) ->
    {{X+1, Y, Z}, Space#{{X, Y, Z} => Char}}.
