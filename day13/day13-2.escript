#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% The game didn't run because you didn't put in any
%% quarters. Unfortunately, you did not bring any quarters. Memory
%% address 0 represents the number of quarters that have been
%% inserted; set it to 2 to play for free.

%% The arcade cabinet has a joystick that can move left and right. The
%% software reads the position of the joystick with input
%% instructions:

%%     If the joystick is in the neutral position, provide 0.
%%     If the joystick is tilted to the left, provide -1.
%%     If the joystick is tilted to the right, provide 1.

%% The arcade cabinet also has a segment display capable of showing a
%% single number that represents the player's current score. When
%% three output instructions specify X=-1, Y=0, the third output
%% instruction is not a tile; the value instead specifies the new
%% score to show in the segment display. For example, a sequence of
%% output values like -1,0,12345 would show 12345 as the player's
%% current score.

%% Beat the game by breaking all the blocks. What is your score after
%% the last block is broken?

-mode('compile').

-export([main/1]).

-define(TILE_EMPTY, 0).
-define(TILE_WALL, 1).
-define(TILE_BLOCK, 2).
-define(TILE_PADDLE, 3).
-define(TILE_BALL, 4).

-define(JOYSTICK_NEUTRAL, 0).
-define(JOYSTICK_LEFT, -1).
-define(JOYSTICK_RIGHT, 1).

-record(game, {screen=#{}
              ,bottom=25, right=45
              ,score=0
              ,paddle_x=23
              ,ball_x=21
              }).

main(_) ->
    Intcode = read_intcode(),
    Twiddled = intcode:twiddle_bit(Intcode, 0, 2), %% free play!

    #game{score=FinalScore} = play_game(Twiddled),

    io:format("final score: ~p~n", [FinalScore]).

play_game(Intcode) ->
    Parent = self(),
    _GamePid = spawn(fun() -> run_game(Intcode, Parent) end),

    update_screen(#game{}).

update_screen(#game{screen=Screen}=Game) ->
    case receive_output() of
        {Child, 'input'} ->
            move_paddle(Game, Child);
        'done' -> Game;
        {-1, 0, NewScore} ->
            update_screen(Game#game{score=NewScore});
        {X, Y, ?TILE_BALL=TileId} ->
            update_screen(Game#game{screen=Screen#{{X, Y} => TileId}
                                   ,ball_x=X
                                   });
        {X, Y, TileId} ->
            update_screen(Game#game{screen=Screen#{{X, Y} => TileId}})
    end.

move_paddle(#game{paddle_x=PaddleX
                 ,ball_x=PaddleX
                 }=Game
           ,Child
           ) ->
    Child ! {'input', ?JOYSTICK_NEUTRAL},
    update_screen(Game);
move_paddle(#game{paddle_x=PaddleX
                 ,ball_x=BallX
                 }=Game
           ,Child
           ) when BallX < PaddleX ->
    Child ! {'input', ?JOYSTICK_LEFT},
    update_screen(Game#game{paddle_x=PaddleX+?JOYSTICK_LEFT});
move_paddle(#game{paddle_x=PaddleX}=Game
           ,Child
           ) ->
    Child ! {'input', ?JOYSTICK_RIGHT},
    update_screen(Game#game{paddle_x=PaddleX+?JOYSTICK_RIGHT}).

print_screen_and_score(#game{score=Score}=Game) ->
    io:format('user', "~nSCORE: ~p~n", [Score]),
    print_screen(Game, {0,0}).

print_screen(#game{screen=Screen
                  ,bottom=Bottom
                  ,right=Right
                  }
            ,{Right, Bottom}
            ) ->
    print_tile(maps:get({Right, Bottom}, Screen, ?TILE_EMPTY)),
    io:format('user', "~n", []);
print_screen(#game{screen=Screen
                  ,right=Right
                  }=Game
            ,{Right, Y}
            ) ->
    print_tile(maps:get({Right, Y}, Screen, ?TILE_EMPTY)),
    io:format('user', "~n", []),
    print_screen(Game, {0, Y+1});
print_screen(#game{screen=Screen}=Game, {X, Y}) ->
    print_tile(maps:get({X, Y}, Screen, ?TILE_EMPTY)),
    print_screen(Game, {X+1, Y}).

print_tile(?TILE_EMPTY) ->  io:format('user', " ", []);
print_tile(?TILE_WALL) ->   io:format('user', "#", []);
print_tile(?TILE_BLOCK) ->  io:format('user', "B", []);
print_tile(?TILE_PADDLE) -> io:format('user', "=", []);
print_tile(?TILE_BALL) ->   io:format('user', "o", []).

receive_output() ->
    receive_output([]).

receive_output([TileId, Y, X]) -> {X, Y, TileId};
receive_output(Output) ->
    receive
        {'output', Data} -> receive_output([Data | Output]);
        {Child, 'input'} -> {Child, 'input'};
        'done' -> 'done'
    end.

run_game(Intcode, Parent) ->
    WithOutput = intcode:set_output_fun(Intcode
                                       ,fun(V, _) -> Parent ! {'output', V} end
                                       ),
    WithInput = intcode:set_input_fun(WithOutput
                                     ,fun() ->
                                              Parent ! {self(), 'input'},
                                              receive
                                                  {'input', Input} -> Input
                                              end
                                      end
                                     ),
    _V = intcode:run(WithInput),
    Parent ! 'done'.

read_intcode() ->
    Contents = read_input(),
    intcode:from_binary(Contents).

read_input() ->
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.
