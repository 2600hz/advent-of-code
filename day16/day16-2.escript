#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% Now that your FFT is working, you can decode the real signal.

%% The real signal is your puzzle input repeated 10000 times. Treat
%% this new signal as a single input list. Patterns are still
%% calculated as before, and 100 phases of FFT are still applied.

%% The first seven digits of your initial input signal also represent
%% the message offset. The message offset is the location of the
%% eight-digit message in the final output list. Specifically, the
%% message offset indicates the number of digits to skip before
%% reading the eight-digit message. For example, if the first seven
%% digits of your initial input signal were 1234567, the eight-digit
%% message would be the eight digits after skipping 1,234,567 digits
%% of the final output list. Or, if the message offset were 7 and your
%% final output list were 98765432109876543210, the eight-digit
%% message would be 21098765. (Of course, your real message offset
%% will be a seven-digit number, not a one-digit number like 7.)

%% Here is the eight-digit message in the final output list after 100
%% phases. The message offset given in each input has been
%% highlighted. (Note that the inputs given below are repeated 10000
%% times to find the actual starting input lists.)

%%     03036732577212944063491565474664 becomes 84462026.
%%     02935109699940807407585447034323 becomes 78725270.
%%     03081770884921959731165446850517 becomes 53553731.

%% After repeating your input signal 10000 times and running 100
%% phases of FFT, what is the eight-digit message embedded in the
%% final output list?

-mode('compile').

-export([main/1]).

main(_) ->
    InputSignal = read_signal(),

    Phases = 100,

    InputNs = length(InputSignal),

    {OutputSignal, InputNs, _} = lists:foldl(fun run_nth_phase/2
                                            ,{InputSignal, InputNs, Patterns}
                                            ,lists:seq(1, Phases)
                                            ),

    MessageOffset = lists:sublist(OutputSignal, 7),
    StartAt = list_to_integer(MessageOffset),
    Message = lists:sublist(OutputSignal, StartAt, 8),

    io:format("~w~n", [Message]).

run_nth_phase(_Nth, {InputSignal, InputNs, Patterns}) ->
    io:format("running phase ~w~n", [_Nth]),
    Pattern = maps:get(InputNs, Patterns),
    {run_phase(InputSignal, InputNs, Pattern), InputNs, Patterns}.

run_pattern(InputSignal, Pattern) ->
    %% offset pattern by 1 position
    {{'value', P}, Ps} = queue:out(queue:from_list(Pattern)),

    {_, V} = lists:foldl(fun run_pattern_fold/2, {queue:in(P, Ps), 0}, InputSignal),
    abs(V rem 10).

generate_patterns(InputNs) ->
    generate_patterns(InputNs, #{}).

generate_patterns(0, Patterns) -> Patterns;
generate_patterns(InputNs, Patterns) ->
    BasePattern = [0, 1, 0, -1],
    generate_patterns(InputNs-1, Patterns#{InputNs => output_pattern(BasePattern, InputNs)}).

output_pattern(Pattern, Nth) ->
    {NthPattern, Nth} = lists:foldl(fun output_pattern_fold/2, {[], Nth}, Pattern),
    NthPattern.

output_pattern_fold(P, {Pattern, N}) ->
    {Pattern ++ lists:duplicate(N, P), N}.

run_pattern_fold(Input, {Pattern, Product}) ->
    {{'value', P}, Ps} = queue:out(Pattern),
    {queue:in(P, Ps), (Input * P) + Product}.

read_signal() ->
    Contents = read_input(),
    binary_to_input_signal(Contents).

binary_to_input_signal(Bin) ->
    Duplicated = lists:duplicate(10000, [C-$0 || <<C>> <= Bin, C =/= $\n]),
    lists:flatten(Duplicated).

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
