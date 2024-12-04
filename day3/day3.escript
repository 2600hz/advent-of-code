#!/usr/bin/env escript
%%! +A0 -sname day3
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([]) ->
    {ok, Input} = file:read_file("puzzle.txt"),
    %% {'ok', Input} = file:read_file("sample.txt"),

    Muls = parse_memory(Input),
    io:format("sum of muls: ~p~n", [lists:sum(Muls)]),

    %% {'ok', Input2} = file:read_file("sample2.txt"),
    CondMuls = parse_memory_cond(Input),
    io:format("sum of cond muls: ~p~n", [lists:sum(CondMuls)]).

parse_memory(Input) ->
    parse_memory(Input, []).

parse_memory(<<>>, Muls) -> Muls;
parse_memory(<<"mul(", Rest/binary>>, Muls) ->
    {Mul, Rest1} = parse_mul(Rest),
    parse_memory(Rest1, [Mul | Muls]);
parse_memory(<<_:1/binary, Rest/binary>>, Muls) ->
    parse_memory(Rest, Muls).

parse_memory_cond(Input) ->
    parse_memory_cond(Input, []).

parse_memory_cond(<<>>, Muls) -> lists:reverse(Muls);
parse_memory_cond(<<"mul(", Rest/binary>>, Muls) ->
    {Mul, Rest1} = parse_mul(Rest),
    parse_memory_cond(Rest1, [Mul | Muls]);
parse_memory_cond(<<"don't()", Rest/binary>>, Muls) ->
    Rest1 = parse_dont(Rest),
    parse_memory_cond(Rest1, Muls);
parse_memory_cond(<<_:1/binary, Rest/binary>>, Muls) ->
    parse_memory_cond(Rest, Muls).

parse_dont(<<>>) -> <<>>;
parse_dont(<<"do()", Rest/binary>>) -> Rest;
parse_dont(<<_:1/binary, Rest/binary>>) ->
    parse_dont(Rest).

parse_mul(Input) ->
    case parse_arg1(Input) of
        {Arg1, Rest1} ->
            case parse_arg2(Rest1) of
                {Arg2, Rest2} -> {Arg1*Arg2, Rest2};
                Rest2 -> {0, Rest2}
            end;
        Rest1 ->
            {0, Rest1}
    end.

parse_arg1(Memory) ->
    parse_arg1(Memory, <<>>).

parse_arg1(<<",", Rest/binary>>, Arg) ->
    {binary_to_integer(Arg), Rest};
parse_arg1(<<N:8, Rest/binary>>, Arg) when N >= $0 andalso N =< $9 ->
    parse_arg1(Rest, <<Arg/binary, N>>);
parse_arg1(Rest, _Arg) ->
    Rest.

parse_arg2(Memory) ->
    parse_arg2(Memory, <<>>).

parse_arg2(<<")", Rest/binary>>, Arg) ->
    {binary_to_integer(Arg), Rest};
parse_arg2(<<N:8, Rest/binary>>, Arg) when N >= $0 andalso N =< $9 ->
    parse_arg2(Rest, <<Arg/binary, N>>);
parse_arg2(Rest, _Arg) ->
    Rest.
