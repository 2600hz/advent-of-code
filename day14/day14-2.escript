#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% For some reason, the sea port's computer system still can't
%% communicate with your ferry's docking program. It must be using
%% version 2 of the decoder chip!

%% A version 2 decoder chip doesn't modify the values being written at
%% all. Instead, it acts as a memory address decoder. Immediately
%% before a value is written to memory, each bit in the bitmask
%% modifies the corresponding bit of the destination memory address in
%% the following way:

%%     If the bitmask bit is 0, the corresponding memory address bit is unchanged.
%%     If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
%%     If the bitmask bit is X, the corresponding memory address bit is floating.

%% A floating bit is not connected to anything and instead fluctuates
%% unpredictably. In practice, this means the floating bits will take
%% on all possible values, potentially causing many memory addresses to
%% be written all at once!

%% For example, consider the following program:

%% mask = 000000000000000000000000000000X1001X
%% mem[42] = 100
%% mask = 00000000000000000000000000000000X0XX
%% mem[26] = 1

%% When this program goes to write to memory address 42, it first
%% applies the bitmask:

%% address: 000000000000000000000000000000101010  (decimal 42)
%% mask:    000000000000000000000000000000X1001X
%% result:  000000000000000000000000000000X1101X

%% After applying the mask, four bits are overwritten, three of which
%% are different, and two of which are floating. Floating bits take on
%% every possible combination of values; with two floating bits, four
%% actual memory addresses are written:

%% 000000000000000000000000000000011010  (decimal 26)
%% 000000000000000000000000000000011011  (decimal 27)
%% 000000000000000000000000000000111010  (decimal 58)
%% 000000000000000000000000000000111011  (decimal 59)

%% Next, the program is about to write to memory address 26 with a
%% different bitmask:

%% address: 000000000000000000000000000000011010  (decimal 26)
%% mask:    00000000000000000000000000000000X0XX
%% result:  00000000000000000000000000000001X0XX

%% This results in an address with three floating bits, causing writes
%% to eight memory addresses:

%% 000000000000000000000000000000010000  (decimal 16)
%% 000000000000000000000000000000010001  (decimal 17)
%% 000000000000000000000000000000010010  (decimal 18)
%% 000000000000000000000000000000010011  (decimal 19)
%% 000000000000000000000000000000011000  (decimal 24)
%% 000000000000000000000000000000011001  (decimal 25)
%% 000000000000000000000000000000011010  (decimal 26)
%% 000000000000000000000000000000011011  (decimal 27)

%% The entire 36-bit address space still begins initialized to the
%% value 0 at every address, and you still need the sum of all values
%% left in memory at the end of the program. In this example, the sum
%% is 208.

%% Execute the initialization program using an emulator for a version 2
%% decoder chip. What is the sum of all values left in memory after it
%% completes?

main(_) ->
    [{'mask', Mask} | Program] = read_input("p14.txt"),
    {_M, Memory} = lists:foldl(fun run_instruction/2, {to_bin_bits(Mask), #{}}, Program),
    Sum = maps:fold(fun(_Loc, V, Acc) -> V + Acc end, 0, Memory),
    io:format("sum: ~p~n", [Sum]).

run_instruction({'mem', Location, Value}, {Mask, Program}) ->
    MemLocations = mem(Location, Mask),
    {Mask, set_locations(Program, Value, MemLocations)};
run_instruction({'mask', Mask}, {_, Program}) ->
    {to_bin_bits(Mask), Program}.

set_locations(Program, Value, MemLocations) ->
    lists:foldl(fun(ML, P) -> P#{ML => Value} end, Program, MemLocations).

mem(Value, Mask) ->
    Ls = mask_values(to_bits(<<Value:36>>), Mask, []),
    [V || <<V:36/integer>> <- lists:flatten(Ls)].

mask_values([], [], VRev) ->
    from_bits(lists:reverse(VRev));
mask_values([V | Vs], [$0 | Ms], VRev) ->
    mask_values(Vs, Ms, [V | VRev]);
mask_values([_V | Vs], [$1 | Ms], VRev) ->
    mask_values(Vs, Ms, [1 | VRev]);
mask_values([_V | Vs], [$X | Ms], VRev) ->
    [mask_values(Vs, Ms, [0 | VRev])
    ,mask_values(Vs, Ms, [1 | VRev])
    ].

to_bits(Bin) ->
    [N || <<N:1>> <= Bin].

to_bin_bits(Bin) ->
    [N || <<N>> <= Bin].

from_bits(List) ->
    << <<N:1>> || N <- List >>.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    parse_program(binary:split(Lines, <<"\n">>, ['global', 'trim'])).

parse_program(Lines) ->
    parse_program(Lines, []).

parse_program([], Program) -> lists:reverse(Program);
parse_program([<<"mask = ", Mask:36/binary>> | Lines], Program) ->
    parse_program(Lines, [{'mask', Mask} | Program]);
parse_program([<<"mem[", MemLine/binary>> | Lines], Program) ->
    {'match', [Location, Value]} =
        re:run(MemLine, <<"(\\d+)[^\\d]+(\\d+)">>, [{'capture', 'all_but_first', 'binary'}]),
    parse_program(Lines, [{'mem', binary_to_integer(Location, 10), binary_to_integer(Value, 10)} | Program]).
