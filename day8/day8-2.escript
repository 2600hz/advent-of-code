#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% After some careful analysis, you believe that exactly one
%% instruction is corrupted.

%% Somewhere in the program, either a jmp is supposed to be a nop, or a
%% nop is supposed to be a jmp. (No acc instructions were harmed in the
%% corruption of this boot code.)

%% The program is supposed to terminate by attempting to execute an
%% instruction immediately after the last instruction in the file. By
%% changing exactly one jmp or nop, you can repair the boot code and
%% make it terminate correctly.

%% For example, consider the same program from above:

%% nop +0
%% acc +1
%% jmp +4
%% acc +3
%% jmp -3
%% acc -99
%% acc +1
%% jmp -4
%% acc +6

%% If you change the first instruction from nop +0 to jmp +0, it would
%% create a single-instruction infinite loop, never leaving that
%% instruction. If you change almost any of the jmp instructions, the
%% program will still eventually find another jmp instruction and loop
%% forever.

%% However, if you change the second-to-last instruction (from jmp -4
%% to nop -4), the program terminates! The instructions are visited in
%% this order:

%% nop +0  | 1
%% acc +1  | 2
%% jmp +4  | 3
%% acc +3  |
%% jmp -3  |
%% acc -99 |
%% acc +1  | 4
%% nop -4  | 5
%% acc +6  | 6

%% After the last instruction (acc +6), the program terminates by
%% attempting to run the instruction below the last instruction in the
%% file. With this change, after the program terminates, the
%% accumulator contains the value 8 (acc +1, acc +1, acc +6).

%% Fix the program so that it terminates normally by changing exactly
%% one jmp (to nop) or nop (to jmp). What is the value of the
%% accumulator after the program terminates?

main(_) ->
    Program = read_input("p8.txt"),

    PossiblePrograms = create_possible_programs(Program),

    Acc = find_halting_program(PossiblePrograms),
    io:format("acc: ~p~n", [Acc]).

create_possible_programs(Program) ->
    {_, Possible} = lists:foldl(fun create_possible_program/2, {Program, []}, maps:keys(Program)),
    Possible.

create_possible_program('eof', Acc) -> Acc;
create_possible_program(Position, {Program, Possibles}=Acc) ->
    case maps:get(Position, Program) of
        {'acc', _} -> Acc;
        {'nop', NOP} -> {Program, [maps:put(Position, {'jmp', NOP}, Program) | Possibles]};
        {'jmp', JMP} -> {Program, [maps:put(Position, {'nop', JMP}, Program) | Possibles]}
    end.

find_halting_program([Program | Possible]) ->
    case detect_acc_at_loop(Program) of
        'undefined' -> find_halting_program(Possible);
        Acc -> Acc
    end.

detect_acc_at_loop(Program) ->
    detect_acc_at_loop(Program, 1, 0, []).

detect_acc_at_loop(#{'eof' := EOFOffset}=Program, Position, Acc, ExecutionSteps) ->
    {NextPosition, NextAcc} = Step = run_instruction(maps:get(Position, Program), Position, Acc),
    case {NextPosition, lists:keyfind(NextPosition, 1, ExecutionSteps)} of
        {EOF, _} when EOFOffset =< EOF ->
            NextAcc; %% found halting program
        {_, 'false'} ->
            detect_acc_at_loop(Program, NextPosition, NextAcc, [Step | ExecutionSteps]);
        {_, {_P, _A}} ->
            'undefined' %% found infinitie loop
    end.

run_instruction({'acc', Inc}, Position, Acc) ->
    {Position+1, Acc+Inc};
run_instruction({'nop', _}, Position, Acc) ->
    {Position+1, Acc};
run_instruction({'jmp', Jump}, Position, Acc) ->
    {Position+Jump, Acc}.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    process_program(binary:split(Lines, <<"\n">>, ['global'])).

process_program(Lines) ->
    {EOFOffset, Program} = lists:foldl(fun process_instruction/2, {1, #{}}, Lines),
    Program#{'eof' => EOFOffset}.

process_instruction(<<>>, Acc) -> Acc;
process_instruction(<<"acc ", Inc/binary>>, {InstructionOffset, Program}) ->
    {InstructionOffset+1
    ,Program#{InstructionOffset => {'acc', binary_to_integer(Inc, 10)}}
    };
process_instruction(<<"nop ", N/binary>>, {InstructionOffset, Program}) ->
    {InstructionOffset+1
    ,Program#{InstructionOffset => {'nop', binary_to_integer(N, 10)}}
    };
process_instruction(<<"jmp ", Jump/binary>>, {InstructionOffset, Program}) ->
    {InstructionOffset+1
    ,Program#{InstructionOffset => {'jmp', binary_to_integer(Jump, 10)}}
    }.
