-module(aoc).

%% Day 1 helpers
-export([mass_fuel_req/1
        ,read_masses/1
        ]).

%% Day 2 helpers
-export([binary_to_intcode/1
        ,intcode_to_binary/1
        ,run_intcode/1
        ]).

mass_fuel_req(Mass) ->
    (Mass div 3) - 2.

read_masses(InputFile) ->
    {'ok', IODevice} = file:open(InputFile, ['read']),
    read_masses(IODevice, file:read_line(IODevice), []).

read_masses(IODevice, 'eof', Masses) ->
    file:close(IODevice),
    Masses;
read_masses(IODevice, {'ok', Line}, Masses) ->
    read_masses(IODevice, file:read_line(IODevice), [list_to_integer(Line -- [$\n]) | Masses]).

%% should take <<"a,b,c">> and create #{0=>a,1=>b,2=>c}
binary_to_intcode(Contents) ->
    Opcodes = binary:split(Contents, <<",">>, ['global']),
    Count = length(Opcodes),
    maps:from_list(lists:zip(lists:seq(0, Count-1), lists:map(fun binary_to_integer/1, Opcodes))).

intcode_to_binary(Intcode) ->
    {_Indicies, Opcodes} = lists:unzip(lists:keysort(1, maps:to_list(Intcode))),
    list_to_binary(lists:join(<<",">>, [integer_to_binary(I) || I <- Opcodes])).

run_intcode(Intcode) ->
    run_intcode(0, Intcode).

run_intcode(InstructionPointer, Intcode) ->
    Opcode = maps:get(InstructionPointer, Intcode),
    process_opcode(InstructionPointer, Intcode, Opcode).

process_opcode(_InstructionPointer, #{0 := Final}, 99) ->
    Final;
process_opcode(InstructionPointer, Intcode, 1) ->
    sum(InstructionPointer, Intcode);
process_opcode(InstructionPointer, Intcode, 2) ->
    multiply(InstructionPointer, Intcode).

step(InstructionPointer, Parameters, Intcode) ->
    run_intcode(InstructionPointer+Parameters, Intcode).

sum(InstructionPointer, Intcode) ->
    NewIntcode = process_instruction(InstructionPointer, Intcode, fun erlang:'+'/2),
    step(InstructionPointer, 4, NewIntcode).

multiply(InstructionPointer, Intcode) ->
    NewIntcode = process_instruction(InstructionPointer, Intcode, fun erlang:'*'/2),
    step(InstructionPointer, 4, NewIntcode).

process_instruction(InstructionPointer, Intcode, Applier) ->
    FirstParameter = maps:get(InstructionPointer+1, Intcode),
    SecondParameter = maps:get(InstructionPointer+2, Intcode),
    StoragePointer = maps:get(InstructionPointer+3, Intcode),

    FirstOperand = maps:get(FirstParameter, Intcode),
    SecondOperand = maps:get(SecondParameter, Intcode),
    Result = Applier(FirstOperand, SecondOperand),

    maps:put(StoragePointer, Result, Intcode).
