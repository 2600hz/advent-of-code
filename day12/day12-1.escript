#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 12: Rain Risk ---

%% Your ferry made decent progress toward the island, but the storm
%% came in faster than anyone expected. The ferry needs to take evasive
%% actions!

%% Unfortunately, the ship's navigation computer seems to be
%% malfunctioning; rather than giving a route directly to safety, it
%% produced extremely circuitous instructions. When the captain uses
%% the PA system to ask if anyone can help, you quickly volunteer.

%% The navigation instructions (your puzzle input) consists of a
%% sequence of single-character actions paired with integer input
%% values. After staring at them for a few minutes, you work out what
%% they probably mean:

%%     Action N means to move north by the given value.
%%     Action S means to move south by the given value.
%%     Action E means to move east by the given value.
%%     Action W means to move west by the given value.
%%     Action L means to turn left the given number of degrees.
%%     Action R means to turn right the given number of degrees.
%%     Action F means to move forward by the given value in the
%%     direction the ship is currently facing.

%% The ship starts by facing east. Only the L and R actions change the
%% direction the ship is facing. (That is, if the ship is facing east
%% and the next instruction is N10, the ship would move north 10 units,
%% but would still move east if the following action were F.)

%% For example:

%% F10
%% N3
%% F7
%% R90
%% F11

%% These instructions would be handled as follows:

%%     F10 would move the ship 10 units east (because the ship starts
%%     by facing east) to east 10, north 0.
%%     N3 would move the ship 3 units north to east 10, north 3.
%%     F7 would move the ship another 7 units east (because the ship is
%%     still facing east) to east 17, north 3.
%%     R90 would cause the ship to turn right by 90 degrees and face
%%     south; it remains at east 17, north 3.
%%     F11 would move the ship 11 units south to east 17, south 8.

%% At the end of these instructions, the ship's Manhattan distance (sum
%% of the absolute values of its east/west position and its north/south
%% position) from its starting position is 17 + 8 = 25.

%% Figure out where the navigation instructions lead. What is the
%% Manhattan distance between that location and the ship's starting
%% position?

-define(EAST, {1, 0}).
-define(WEST, {-1, 0}).
-define(NORTH, {0, 1}).
-define(SOUTH, {0, -1}).

-record(ship, {heading = ?EAST
              ,position = {0, 0}
              }
       ).

main(_) ->
    Actions = read_input("p12.txt"),
    #ship{position = {X, Y}} =
        lists:foldl(fun move_ship/2, #ship{}, Actions),
    io:format("manhat: ~p + ~p = ~p~n", [X, Y, (abs(X)+abs(Y))]).

%%     Action N means to move north by the given value.
%%     Action S means to move south by the given value.
%%     Action E means to move east by the given value.
%%     Action W means to move west by the given value.
%%     Action L means to turn left the given number of degrees.
%%     Action R means to turn right the given number of degrees.
%%     Action F means to move forward by the given value in the
%%     direction the ship is currently facing.

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

move_by_heading(#ship{heading={Hx, Hy}
                     ,position={X, Y}
                     }=Ship
               ,Distance
               ) ->
    Ship#ship{position={X + (Hx * Distance)
                       ,Y + (Hy * Distance)
                       }
             }.

adjust_heading(#ship{heading=Heading}=Ship, Action, Degrees) ->
    Ship#ship{heading=move_heading(Heading, Action, Degrees div 90 rem 4)}.

move_heading(Heading, _Action, 0) -> Heading;
move_heading(Heading, $L, N) ->
    move_counter(Heading, N);
move_heading(Heading, $R, N) ->
    move_clock(Heading, N).

move_counter(?EAST, 1) -> ?NORTH;
move_counter(?NORTH, 1) -> ?WEST;
move_counter(?WEST, 1) -> ?SOUTH;
move_counter(?SOUTH, 1) -> ?EAST;
move_counter(Pos, N) ->
    move_counter(move_counter(Pos, 1), N-1).

move_clock(?EAST, 1) -> ?SOUTH;
move_clock(?NORTH, 1) -> ?EAST;
move_clock(?WEST, 1) -> ?NORTH;
move_clock(?SOUTH, 1) -> ?WEST;
move_clock(Pos, N) ->
    move_clock(move_clock(Pos, 1), N-1).

move_position(#ship{position={X, Y}}=Ship, {Dx, Dy}) ->
    Ship#ship{position={X+Dx, Y+Dy}}.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [parse_instruction(Line) || Line <- binary:split(Lines, <<"\n">>, ['global', 'trim'])].

parse_instruction(<<Action:8, Value/binary>>) ->
    {Action, binary_to_integer(Value, 10)}.
