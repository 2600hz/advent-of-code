#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% You now have a complete Intcode computer.

%% Finally, you can lock on to the Ceres distress signal! You just
%% need to boost your sensors using the BOOST program.

%% The program runs in sensor boost mode by providing the input
%% instruction the value 2. Once run, it will boost the sensors
%% automatically, but it might take a few seconds to complete the
%% operation on slower hardware. In sensor boost mode, the program
%% will output a single value: the coordinates of the distress signal.

%% Run the BOOST program in sensor boost mode. What are the
%% coordinates of the distress signal?

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    Intcode = intcode:set_input_fun(read_intcode()
                                   ,fun() -> 2 end
                                   ),
    Return = intcode:run(Intcode),
    io:format("~ndone: ~p~n", [Return]).

read_intcode() ->
    Contents = read_input(),
    intcode:from_binary(Contents).

read_input() ->
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.

test_input() ->
    %% <<"104,1125899906842624,99">>.
    %% <<"1102,34915192,34915192,7,4,7,99,0">>.
    <<"109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99">>.
