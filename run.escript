#!/usr/bin/env escript
%%! +A0 -sname advent -pa ebin
%% -*- coding: utf-8 -*-
-mode('compile').

-export([main/1]).

main(Modules) ->
    lists:foreach(fun log_and_run_mod/1, Modules).

log_and_run_mod(Module) ->
    M = list_to_atom(Module),
    case ensure_loaded(M) andalso erlang:function_exported(M, 'run', 0) of
        'false' -> 'ok';
        'true' ->
            io:format("running ~s~n", [Module]),
            run_mod(M)
    end.

run_mod(Module) ->
    Start = erlang:monotonic_time(),
    PidRef = spawn_monitor(Module, 'run', []),
    wait_for_mod(Module, Start, PidRef).

wait_for_mod(Module, Start, {Pid, Ref}) ->
    receive
        {'DOWN', Ref, 'process', Pid, 'normal'} ->
            End = erlang:monotonic_time(),
            Elapsed = erlang:convert_time_unit(End-Start, 'native', 'microsecond'),
            io:format("~n~p: ~pus~n", [Module, Elapsed]);
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            End = erlang:monotonic_time(),
            Elapsed = erlang:convert_time_unit(End-Start, 'native', 'microsecond'),
            io:format("~n~p: down after ~pus~n~p~n", [Module, Elapsed, _Reason]);
        _Msg ->
            io:format('user', "msg: ~p~n", [_Msg]),
            wait_for_mod(Module, Start, {Pid, Ref})
    after 120000 ->
            exit(Pid, 'kill'),
            End = erlang:monotonic_time(),
            Elapsed = erlang:convert_time_unit(End-Start, 'native', 'microsecond'),
            io:format("killed ~s(~p) after ~pus~n", [Module, Pid, Elapsed])
    end.

ensure_loaded(M) ->
    case erlang:module_loaded(M) of
        'true' -> 'true';
        'false' ->
            {'module', M} = code:ensure_loaded(M),
            'true'
    end.
