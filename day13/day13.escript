#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Day 13: Care Package ---

%% As you ponder the solitude of space and the ever-increasing
%% three-hour roundtrip for messages between you and Earth, you notice
%% that the Space Mail Indicator Light is blinking. To help keep you
%% sane, the Elves have sent you a care package.

%% It's a new game for the ship's arcade cabinet! Unfortunately, the
%% arcade is all the way on the other end of the ship. Surely, it
%% won't be hard to build your own - the care package even comes with
%% schematics.

%% The arcade cabinet runs Intcode software like the game the Elves
%% sent (your puzzle input). It has a primitive screen capable of
%% drawing square tiles on a grid. The software draws tiles to the
%% screen with output instructions: every three output instructions
%% specify the x position (distance from the left), y position
%% (distance from the top), and tile id. The tile id is interpreted as
%% follows:

%%     0 is an empty tile. No game object appears in this tile.
%%     1 is a wall tile. Walls are indestructible barriers.
%%     2 is a block tile. Blocks can be broken by the ball.
%%     3 is a horizontal paddle tile. The paddle is indestructible.
%%     4 is a ball tile. The ball moves diagonally and bounces off objects.

%% For example, a sequence of output values like 1,2,3,6,5,4 would
%% draw a horizontal paddle tile (1 tile from the left and 2 tiles
%% from the top) and a ball tile (6 tiles from the left and 5 tiles
%% from the top).

%% Start the game. How many block tiles are on the screen when the
%% game exits?

-mode('compile').

-export([main/1]).

-define(TILE_EMPTY, 0).
-define(TILE_WALL, 1).
-define(TILE_BLOCK, 2).
-define(TILE_PADDLE, 3).
-define(TILE_BALL, 4).

main(_) ->
    Intcode = read_intcode(),
    FinalScreen = play_game(Intcode),
    Blocks = maps:fold(fun({_X, _Y}, ?TILE_BLOCK, Acc) -> Acc+1;
                          (_, _, Acc) -> Acc
                       end
                      ,0
                      ,FinalScreen
                      ),
    io:format("blocks painted: ~p~n", [Blocks]).

play_game(Intcode) ->
    Parent = self(),
    _GamePid = spawn(fun() -> run_game(Intcode, Parent) end),

    update_screen(#{}).

update_screen(Screen) ->
    case receive_output() of
        'done' -> Screen;
        {X, Y, TileId} -> update_screen(Screen#{{X, Y} => TileId})
    end.

receive_output() ->
    receive_output([]).

receive_output([TileId, Y, X]) -> {X, Y, TileId};
receive_output(Output) ->
    receive
        {'output', Data} -> receive_output([Data | Output]);
        'done' -> 'done'
    end.

run_game(Intcode, Parent) ->
    WithOutput = intcode:set_output_fun(Intcode
                                       ,fun(V, _) -> Parent ! {'output', V} end
                                       ),
    _V = intcode:run(WithOutput),
    Parent ! 'done'.

read_intcode() ->
    Contents = read_input(),
    intcode:from_binary(Contents).

read_input() ->
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.
