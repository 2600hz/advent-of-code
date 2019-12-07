#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% It's no good - in this configuration, the amplifiers can't generate
%% a large enough output signal to produce the thrust you'll need. The
%% Elves quickly talk you through rewiring the amplifiers into a
%% feedback loop:

%%       O-------O  O-------O  O-------O  O-------O  O-------O
%% 0 -+->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-.
%%    |  O-------O  O-------O  O-------O  O-------O  O-------O |
%%    |                                                        |
%%    '--------------------------------------------------------+
%%                                                             |
%%                                                             v
%%                                                      (to thrusters)

%% Most of the amplifiers are connected as they were before; amplifier
%% A's output is connected to amplifier B's input, and so on. However,
%% the output from amplifier E is now connected into amplifier A's
%% input. This creates the feedback loop: the signal will be sent
%% through the amplifiers many times.

%% In feedback loop mode, the amplifiers need totally different phase
%% settings: integers from 5 to 9, again each used exactly once. These
%% settings will cause the Amplifier Controller Software to repeatedly
%% take input and produce output many times before halting. Provide
%% each amplifier its phase setting at its first input instruction;
%% all further input/output instructions are for signals.

%% Don't restart the Amplifier Controller Software on any amplifier
%% during this process. Each one should continue receiving and sending
%% signals until it halts.

%% All signals sent or received in this process will be between pairs
%% of amplifiers except the very first signal and the very last
%% signal. To start the process, a 0 signal is sent to amplifier A's
%% input exactly once.

%% Eventually, the software on the amplifiers will halt after they
%% have processed the final loop. When this happens, the last output
%% signal from amplifier E is sent to the thrusters. Your job is to
%% find the largest output signal that can be sent to the thrusters
%% using the new phase settings and feedback loop arrangement.

%% Here are some example programs:

%%     Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5):

%%     3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
%%     27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5

%%     Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6):

%%     3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
%%     -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
%%     53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10

%% Try every combination of the new phase settings on the amplifier
%% feedback loop. What is the highest signal that can be sent to the
%% thrusters?

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    Intcode = read_intcode(),

    Amps = lists:duplicate(5, Intcode),
    AllPhasePermutations = aoc:permutations(lists:seq(5,9)),

    {_, HighestSignal} =
        lists:foldl(fun calc_output_signal/2
                   ,{Amps, 0}
                   ,AllPhasePermutations
                   ),

    %% Phases = [9,8,7,6,5],
    %% HighestSignal = calc_output_signal(Phases, {Amps, 0}),
    io:format("highest output signal: ~p~n", [HighestSignal]).

calc_output_signal(Phases, {Amps, OutputSignal}) ->
    PhasedAmps = lists:zip(Phases, Amps),
    PidRefs = lists:foldl(fun start_amps/2, [], PhasedAmps),
    [{AmpAPid, AmpARef} | _]=AmpPidRefs = lists:reverse(PidRefs),
    timer:sleep(10),
    %% io:format("started Amp PIDS~n~n"),
    notify_of_next_pid(AmpPidRefs ++ [{AmpAPid, AmpARef}]),
    %% io:format("sending signal 0 to ~p~n~n", [AmpAPid]),
    AmpAPid ! {'input_signal', 0},
    case wait_for_pids(AmpAPid, AmpPidRefs) of
        NewHighest when NewHighest > OutputSignal ->
            {Amps, NewHighest};
        _Signal -> {Amps, OutputSignal}
    end.

wait_for_pids(AmpAPid, []) ->
    receive
        {'output_signal', AmpAPid, OutputSignal} -> OutputSignal
    end;
wait_for_pids(AmpAPid, [{Pid, Ref} | PidRefs]) ->
    receive
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            %% io:format("~p done~n", [Pid]),
            wait_for_pids(AmpAPid, PidRefs);
        {'output_signal', AmpAPid, OutputSignal} -> OutputSignal
    end.

notify_of_next_pid([_]) -> 'ok';
notify_of_next_pid([{A, _}, {B, BRef} | Rest]) ->
    A ! {'next_amp', B},
    notify_of_next_pid([{B, BRef} | Rest]).

start_amps({Phase, Amp}, AmpPids) ->
    TopParent = self(),
    {_AmpPid, _}=PidRef = spawn_monitor(fun() -> start_amp(TopParent, Phase, Amp) end),
    %% io:format("starting amp phase ~p: ~p~n", [Phase, AmpPid]),
    [PidRef | AmpPids].

start_amp(TopParent, Phase, Amp) ->
    receive
        {'next_amp', NextAmpPid} ->
            %% io:format("~p will send output to ~p~n", [self(), NextAmpPid]),
            NextAmpRef = monitor('process', NextAmpPid),

            Parent = self(),
            AmpOutput = intcode:set_output_fun(Amp, fun(Value, _) ->
                                                            %% io:format("r(~p) sending output to ~p: ~p~n", [self(), Parent, Value]),
                                                            Parent ! {'output_signal', Value}
                                                    end),
            wait_for_start(TopParent, Phase, AmpOutput, {NextAmpPid, NextAmpRef})
    end.

wait_for_start(TopParent, Phase, Amp, {NextAmpPid, NextAmpRef}) ->
    receive
        {'input_signal', InputSignal} ->
            %% io:format("~p input signal ~p recv~n", [self(), InputSignal]),
            Parent = self(),
            {AmpPid, AmpRef} = spawn_monitor(fun() -> run_phase_signal(Parent, Amp) end),
            %% io:format("~p spawned intcode runner in r(~p)~n", [self(), AmpPid]),
            wait_for_input_loop(TopParent, {AmpPid, AmpRef}, {NextAmpPid, NextAmpRef}, [Phase, InputSignal])
    end.

wait_for_input_loop(TopParent, {AmpPid, AmpRef}=PidRef, {NextAmpPid, NextAmpRef}, Inputs) ->
    receive
        {Child, 'child_input'} when Inputs =:= [] ->
            self() ! {Child, 'child_input'},
            wait_for_input_loop(TopParent, PidRef, {NextAmpPid, NextAmpRef}, Inputs);
        {Child, 'child_input'} ->
            Child ! {self(), hd(Inputs)},
            %% io:format("~p telling runner r(~p) of ~p~n", [self(), Child, hd(Inputs)]),
            wait_for_input_loop(TopParent, PidRef, {NextAmpPid, NextAmpRef}, tl(Inputs));
        {'output_signal', NewOutputSignal} ->
            %% io:format("~p recv output signal ~p from runner, sending to ~p~n", [self(), NewOutputSignal, NextAmpPid]),
            NextAmpPid ! {'input_signal', NewOutputSignal},
            wait_for_input_loop(TopParent, PidRef, {NextAmpPid, NextAmpRef}, Inputs);
        {'input_signal', InputSignal} when AmpPid =:= 'undefined' ->
            %% io:format("~p input signal ~p sent to ~p~n", [self(), InputSignal, TopParent]),
            TopParent ! {'output_signal', self(), InputSignal};
        {'input_signal', InputSignal} ->
            %% io:format("~p input signal ~p~n", [self(), InputSignal]),
            wait_for_input_loop(TopParent, PidRef, {NextAmpPid, NextAmpRef}, [InputSignal]);
        {'DOWN', AmpRef, 'process', AmpPid, _Reason} ->
            %% io:format("~p r(~p) down: final: ~p~n", [self(), AmpPid, Inputs]),
            wait_for_input_loop(TopParent, {'undefined', 'undefined'}, {NextAmpPid, NextAmpRef}, Inputs);
        {'DOWN', NextAmpRef, 'process', NextAmpPid, _Reason} ->
            %% io:format("~p next amp ~p down~n", [self(), NextAmpPid]),
            wait_for_input_loop(TopParent, {AmpPid, AmpRef}, {'undefined', 'undefined'}, Inputs)
    end.

run_phase_signal(Parent, Intcode) ->
    IntcodeWithInput = intcode:set_input_fun(Intcode
                                            ,fun() ->
                                                     %% io:format("r(~p) asking for input from ~p~n", [self(), Parent]),
                                                     Parent ! {self(), 'child_input'},
                                                     receive
                                                         {Parent, Input} -> Input
                                                     end
                                             end
                                            ),
    %% io:format("r(~p) running intcode~n", [self()]),
    intcode:run(IntcodeWithInput).

read_input() ->
    %% test_input().
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.

read_intcode() ->
    intcode:from_binary(read_input()).

%% test_input() ->
%%     <<"3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5">>. %% Phases = [9,8,7,6,5] => 139629729
