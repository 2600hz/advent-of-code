-module(day2).
-export([test_new_computer/0, find_input_pair_checksum/0]).

-define(OPCODE_HALT, 99).
-define(OPCODE_SUM, 1).
-define(OPCODE_PRODUCT, 2).

read_input(InputFile) ->
    {'ok', Input} = file:read_file(InputFile),
    binary:part(Input, 0, byte_size(Input) - 1).

binary_to_intcode(Binary) ->
    Intcode = binary:split(Binary, <<",">>, ['global']),
    IntcodeLen = length(Intcode),
    maps:from_list(lists:zip(lists:seq(0, IntcodeLen - 1), [ binary_to_integer(X) || X <- Intcode])).

run_intcode(Intcode) ->
    run_intcode(Intcode, 0).

run_intcode(Intcode, InstructionPointer) ->
    #{InstructionPointer := Opcode} = Intcode,
    process_intcode(Intcode, InstructionPointer, Opcode).

process_intcode(#{0 := Output}, _InstructionPointer, ?OPCODE_HALT) ->
    Output;
process_intcode(Intcode, InstructionPointer, ?OPCODE_SUM) ->
    sum(Intcode, InstructionPointer);
process_intcode(Intcode, InstructionPointer, ?OPCODE_PRODUCT) ->
    product(Intcode, InstructionPointer).

sum(Intcode, InstructionPointer) ->
    NewIntcode = process_instruction(Intcode, InstructionPointer, fun erlang:'+'/2),
    step(NewIntcode, InstructionPointer, 4).

product(Intcode, InstructionPointer) ->
    NewIntcode = process_instruction(Intcode, InstructionPointer, fun erlang:'*'/2),
    step(NewIntcode, InstructionPointer, 4).

step(Intcode, InstructionPointer, Instructions) ->
    run_intcode(Intcode, InstructionPointer + Instructions).

process_instruction(Intcode, InstructionPointer, Applier) ->
    ValuePointer1 = maps:get(InstructionPointer + 1, Intcode),
    ValuePointer2 = maps:get(InstructionPointer + 2, Intcode),
    StoragePointer = maps:get(InstructionPointer + 3, Intcode),

    #{ ValuePointer1 := Value1 } = Intcode,
    #{ ValuePointer2 := Value2 } = Intcode,
    Result = Applier(Value1, Value2),
    Intcode#{ StoragePointer := Result}.

reset_instructions(Intcode, Noun, Verb) ->
    Intcode#{ 1 := Noun, 2 := Verb}.

find_noun_and_verb(ExpectedOutput, MaxSearch) ->
    Intcode = binary_to_intcode(read_input("day2.input")),
    find_noun_and_verb(Intcode, ExpectedOutput, 'null', 1, 1, MaxSearch).

find_noun_and_verb(_Intcode, ExpectedOutput, ExpectedOutput, NounValue, VerbValue, _MaxSearch) ->
    (NounValue * 100) + (VerbValue - 1);
find_noun_and_verb(_Intcode, _ExpectedOutput, _Output, MaxSearch, MaxSearch, MaxSearch) ->
    false;
find_noun_and_verb(Intcode, ExpectedOutput, Output, NounValue, MaxSearch, MaxSearch) ->
    find_noun_and_verb(Intcode, ExpectedOutput, Output, NounValue + 1, 1, MaxSearch);
find_noun_and_verb(Intcode, ExpectedOutput, _Output, NounValue, VerbValue, MaxSearch) ->
    NewOutput = run_intcode(reset_instructions(Intcode, NounValue, VerbValue)),
    find_noun_and_verb(Intcode, ExpectedOutput, NewOutput, NounValue, VerbValue + 1, MaxSearch).

test_new_computer() ->
    Intcode = reset_instructions(binary_to_intcode(read_input("day2.input")), 12, 2),
    run_intcode(Intcode).

find_input_pair_checksum() ->
    find_noun_and_verb(19690720, 99).
