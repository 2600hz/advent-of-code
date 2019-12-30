#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Day 16: Flawed Frequency Transmission ---

%% You're 3/4ths of the way through the gas giants. Not only do
%% roundtrip signals to Earth take five hours, but the signal quality
%% is quite bad as well. You can clean up the signal with the Flawed
%% Frequency Transmission algorithm, or FFT.

%% As input, FFT takes a list of numbers. In the signal you received
%% (your puzzle input), each number is a single digit: data like 15243
%% represents the sequence 1, 5, 2, 4, 3.

%% FFT operates in repeated phases. In each phase, a new list is
%% constructed with the same length as the input list. This new list
%% is also used as the input for the next phase.

%% Each element in the new list is built by multiplying every value in
%% the input list by a value in a repeating pattern and then adding up
%% the results. So, if the input list were 9, 8, 7, 6, 5 and the
%% pattern for a given element were 1, 2, 3, the result would be 9*1 +
%% 8*2 + 7*3 + 6*1 + 5*2 (with each input element on the left and each
%% value in the repeating pattern on the right of each
%% multiplication). Then, only the ones digit is kept: 38 becomes 8,
%% -17 becomes 7, and so on.

%% While each element in the output array uses all of the same input
%% array elements, the actual repeating pattern to use depends on
%% which output element is being calculated. The base pattern is 0, 1,
%% 0, -1. Then, repeat each value in the pattern a number of times
%% equal to the position in the output list being considered. Repeat
%% once for the first element, twice for the second element, three
%% times for the third element, and so on. So, if the third element of
%% the output list is being calculated, repeating the values would
%% produce: 0, 0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1.

%% When applying the pattern, skip the very first value exactly
%% once. (In other words, offset the whole pattern left by one.) So,
%% for the second element of the output list, the actual pattern used
%% would be: 0, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, -1, ....

%% After using this process to calculate each element of the output
%% list, the phase is complete, and the output list of this phase is
%% used as the new input list for the next phase, if any.

%% Given the input signal 12345678, below are four phases of
%% FFT. Within each phase, each output digit is calculated on a single
%% line with the result at the far right; each multiplication
%% operation shows the input digit on the left and the pattern value
%% on the right:

%% Input signal: 12345678

%% 1*1  + 2*0  + 3*-1 + 4*0  + 5*1  + 6*0  + 7*-1 + 8*0  = 4
%% 1*0  + 2*1  + 3*1  + 4*0  + 5*0  + 6*-1 + 7*-1 + 8*0  = 8
%% 1*0  + 2*0  + 3*1  + 4*1  + 5*1  + 6*0  + 7*0  + 8*0  = 2
%% 1*0  + 2*0  + 3*0  + 4*1  + 5*1  + 6*1  + 7*1  + 8*0  = 2
%% 1*0  + 2*0  + 3*0  + 4*0  + 5*1  + 6*1  + 7*1  + 8*1  = 6
%% 1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*1  + 7*1  + 8*1  = 1
%% 1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*0  + 7*1  + 8*1  = 5
%% 1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*0  + 7*0  + 8*1  = 8

%% After 1 phase: 48226158

%% 4*1  + 8*0  + 2*-1 + 2*0  + 6*1  + 1*0  + 5*-1 + 8*0  = 3
%% 4*0  + 8*1  + 2*1  + 2*0  + 6*0  + 1*-1 + 5*-1 + 8*0  = 4
%% 4*0  + 8*0  + 2*1  + 2*1  + 6*1  + 1*0  + 5*0  + 8*0  = 0
%% 4*0  + 8*0  + 2*0  + 2*1  + 6*1  + 1*1  + 5*1  + 8*0  = 4
%% 4*0  + 8*0  + 2*0  + 2*0  + 6*1  + 1*1  + 5*1  + 8*1  = 0
%% 4*0  + 8*0  + 2*0  + 2*0  + 6*0  + 1*1  + 5*1  + 8*1  = 4
%% 4*0  + 8*0  + 2*0  + 2*0  + 6*0  + 1*0  + 5*1  + 8*1  = 3
%% 4*0  + 8*0  + 2*0  + 2*0  + 6*0  + 1*0  + 5*0  + 8*1  = 8

%% After 2 phases: 34040438

%% 3*1  + 4*0  + 0*-1 + 4*0  + 0*1  + 4*0  + 3*-1 + 8*0  = 0
%% 3*0  + 4*1  + 0*1  + 4*0  + 0*0  + 4*-1 + 3*-1 + 8*0  = 3
%% 3*0  + 4*0  + 0*1  + 4*1  + 0*1  + 4*0  + 3*0  + 8*0  = 4
%% 3*0  + 4*0  + 0*0  + 4*1  + 0*1  + 4*1  + 3*1  + 8*0  = 1
%% 3*0  + 4*0  + 0*0  + 4*0  + 0*1  + 4*1  + 3*1  + 8*1  = 5
%% 3*0  + 4*0  + 0*0  + 4*0  + 0*0  + 4*1  + 3*1  + 8*1  = 5
%% 3*0  + 4*0  + 0*0  + 4*0  + 0*0  + 4*0  + 3*1  + 8*1  = 1
%% 3*0  + 4*0  + 0*0  + 4*0  + 0*0  + 4*0  + 3*0  + 8*1  = 8

%% After 3 phases: 03415518

%% 0*1  + 3*0  + 4*-1 + 1*0  + 5*1  + 5*0  + 1*-1 + 8*0  = 0
%% 0*0  + 3*1  + 4*1  + 1*0  + 5*0  + 5*-1 + 1*-1 + 8*0  = 1
%% 0*0  + 3*0  + 4*1  + 1*1  + 5*1  + 5*0  + 1*0  + 8*0  = 0
%% 0*0  + 3*0  + 4*0  + 1*1  + 5*1  + 5*1  + 1*1  + 8*0  = 2
%% 0*0  + 3*0  + 4*0  + 1*0  + 5*1  + 5*1  + 1*1  + 8*1  = 9
%% 0*0  + 3*0  + 4*0  + 1*0  + 5*0  + 5*1  + 1*1  + 8*1  = 4
%% 0*0  + 3*0  + 4*0  + 1*0  + 5*0  + 5*0  + 1*1  + 8*1  = 9
%% 0*0  + 3*0  + 4*0  + 1*0  + 5*0  + 5*0  + 1*0  + 8*1  = 8

%% After 4 phases: 01029498

%% Here are the first eight digits of the final output list after 100
%% phases for some larger inputs:

%%     8087122458591454661908321864

%%     5595 becomes 24176176.
%%     19617804207202209144916044189917 becomes 73745418.
%%     69317163492948606335995924319873 becomes 52432133.

%% After 100 phases of FFT, what are the first eight digits in the
%% final output list?

-mode('compile').

-export([main/1]).

main(_) ->
    InputSignal = read_signal(),
    Phases = 100,

    OutputSignal = lists:foldl(fun run_nth_phase/2, InputSignal, lists:seq(1, Phases)),

    Answer = lists:sublist(OutputSignal, 8),

    io:format("~w~n", [Answer]).

run_nth_phase(_Nth, InputSignal) ->
    run_phase(InputSignal).

run_phase(InputSignal) ->
    BasePattern = [0, 1, 0, -1],
    run_phase(InputSignal, BasePattern).

run_phase(InputSignal, Pattern) ->
    run_phase(InputSignal, Pattern, length(InputSignal), [], 1).

run_phase(InputSignal, Pattern, OutputN, OutputSignal, OutputN) ->
    V = run_pattern(InputSignal, output_pattern(Pattern, OutputN)),
    lists:reverse([V | OutputSignal]);
run_phase(InputSignal, Pattern, InputN, OutputSignal, OutputN) ->
    V = run_pattern(InputSignal, output_pattern(Pattern, OutputN)),
    run_phase(InputSignal, Pattern, InputN, [V | OutputSignal], OutputN+1).

run_pattern(InputSignal, Pattern) ->
    %% offset pattern by 1 position
    {{'value', P}, Ps} = queue:out(queue:from_list(Pattern)),
    run_pattern(InputSignal, queue:in(P, Ps), []).

output_pattern(Pattern, Nth) ->
    {NthPattern, Nth} = lists:foldl(fun output_pattern_fold/2, {[], Nth}, Pattern),
    NthPattern.

output_pattern_fold(P, {Pattern, N}) ->
    {Pattern ++ lists:duplicate(N, P), N}.

run_pattern([], _Pattern, Products) ->
    P = lists:sum(Products),
    abs(P rem 10);
run_pattern([Input | Inputs], Pattern, Products) ->
    {{'value', P}, Ps} = queue:out(Pattern),
    run_pattern(Inputs, queue:in(P, Ps), [Input * P | Products]).

read_signal() ->
    Contents = read_input(),
    binary_to_input_signal(Contents).

binary_to_input_signal(Bin) ->
    [C-$0 || <<C>> <= Bin, C =/= $\n].

read_input() ->
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.

%% test_input() ->
%% <<"12345678">>.
%% <<"80871224585914546619083218645595">>. % 100 phases = 24176176
%% <<"19617804207202209144916044189917">>. % 100 phases = 73745418
%% <<"69317163492948606335995924319873">>. % 100 phases = 52432133
