#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% Before you can give the destination to the captain, you realize that
%% the actual action meanings were printed on the back of the
%% instructions the whole time.

%% Almost all of the actions indicate how to move a waypoint which is
%% relative to the ship's position:

%%     Action N means to move the waypoint north by the given value.
%%     Action S means to move the waypoint south by the given value.
%%     Action E means to move the waypoint east by the given value.
%%     Action W means to move the waypoint west by the given value.
%%     Action L means to rotate the waypoint around the ship left
%%     (counter-clockwise) the given number of degrees.
%%     Action R means to rotate the waypoint around the ship right
%%     (clockwise) the given number of degrees.
%%     Action F means to move forward to the waypoint a number of times
%%     equal to the given value.

%% The waypoint starts 10 units east and 1 unit north relative to the
%% ship. The waypoint is relative to the ship; that is, if the ship
%% moves, the waypoint moves with it.

%% For example, using the same instructions as above:

%%     F10 moves the ship to the waypoint 10 times (a total of 100
%%     units east and 10 units north), leaving the ship at east 100,
%%     north 10. The waypoint stays 10 units east and 1 unit north of
%%     the ship.
%%     N3 moves the waypoint 3 units north to 10 units east and 4 units
%%     north of the ship. The ship remains at east 100, north 10.
%%     F7 moves the ship to the waypoint 7 times (a total of 70 units
%%     east and 28 units north), leaving the ship at east 170, north
%%     38. The waypoint stays 10 units east and 4 units north of the
%%     ship.
%%     R90 rotates the waypoint around the ship clockwise 90 degrees,
%%     moving it to 4 units east and 10 units south of the ship. The
%%     ship remains at east 170, north 38.
%%     F11 moves the ship to the waypoint 11 times (a total of 44 units
%%     east and 110 units south), leaving the ship at east 214, south
%%     72. The waypoint stays 4 units east and 10 units south of the
%%     ship.

%% After these operations, the ship's Manhattan distance from its
%% starting position is 214 + 72 = 286.

%% Figure out where the navigation instructions actually lead. What is
%% the Manhattan distance between that location and the ship's starting
%% position?

-define(EAST, {1, 0}).
-define(WEST, {-1, 0}).
-define(NORTH, {0, 1}).
-define(SOUTH, {0, -1}).

-record(ship, {position = {0, 0}
              ,waypoint = {10, 1} % 10 east, 1 north
              }
       ).

main(_) ->
    Actions = read_input("p12.txt"),
    #ship{position = {X, Y}} =
        lists:foldl(fun move_ship/2, #ship{}, Actions),
    io:format("manhat: ~p + ~p = ~p~n", [X, Y, (abs(X)+abs(Y))]).

%%     Action N means to move the waypoint north by the given value.
%%     Action S means to move the waypoint south by the given value.
%%     Action E means to move the waypoint east by the given value.
%%     Action W means to move the waypoint west by the given value.
%%     Action L means to rotate the waypoint around the ship left
%%     (counter-clockwise) the given number of degrees.
%%     Action R means to rotate the waypoint around the ship right
%%     (clockwise) the given number of degrees.
move_ship({$N, Value}, Ship) ->
    move_position(Ship, {0, Value});
move_ship({$E, Value}, Ship) ->
    move_position(Ship, {Value, 0});
move_ship({$S, Value}, Ship) ->
    move_position(Ship, {0, -Value});
move_ship({$W, Value}, Ship) ->
    move_position(Ship, {-Value, 0});
move_ship({$L, Degrees}, Ship) ->
    adjust_heading(Ship, $L, Degrees);
move_ship({$R, Degrees}, Ship) ->
    adjust_heading(Ship, $R, Degrees);
move_ship({$F, Value}, Ship) ->
    move_by_heading(Ship, Value).

%%     Action F means to move forward to the waypoint a number of times
%%     equal to the given value.
move_by_heading(#ship{position={X, Y}
                     ,waypoint={Wx, Wy}
                     }=Ship
               ,Distance
               ) ->
    Nx = X + (Wx*Distance),
    Ny = Y + (Wy*Distance),
    Ship#ship{position={Nx, Ny}}.

adjust_heading(#ship{waypoint=Waypoint}=Ship, Action, Degrees) ->
    Ship#ship{waypoint=move_heading(Waypoint, Action, Degrees div 90 rem 4)}.

move_heading(Waypoint, _Action, 0) -> Waypoint;
move_heading(Waypoint, $L, N) ->
    move_counter(Waypoint, N);
move_heading(Waypoint, $R, N) ->
    move_clock(Waypoint, N).

move_counter({X, Y}, 1) -> {-Y, X};
move_counter(Pos, N) ->
    move_counter(move_counter(Pos, 1), N-1).

move_clock({X, Y}, 1) -> {Y , -X};
move_clock(Pos, N) ->
    move_clock(move_clock(Pos, 1), N-1).

move_position(#ship{waypoint={X, Y}}=Ship, {Dx, Dy}) ->
    Ship#ship{waypoint={X+Dx, Y+Dy}}.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [parse_instruction(Line) || Line <- binary:split(Lines, <<"\n">>, ['global', 'trim'])].

parse_instruction(<<Action:8, Value/binary>>) ->
    {Action, binary_to_integer(Value, 10)}.
