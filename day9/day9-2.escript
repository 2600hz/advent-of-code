#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% The final step in breaking the XMAS encryption relies on the invalid
%% number you just found: you must find a contiguous set of at least
%% two numbers in your list which sum to the invalid number from step
%% 1.

%% Again consider the above example:

%% 35
%% 20
%% 15
%% 25
%% 47
%% 40
%% 62
%% 55
%% 65
%% 95
%% 102
%% 117
%% 150
%% 182
%% 127
%% 219
%% 299
%% 277
%% 309
%% 576

%% In this list, adding up all of the numbers from 15 through 40
%% produces the invalid number from step 1, 127. (Of course, the
%% contiguous set of numbers in your actual list might be much longer.)

%% To find the encryption weakness, add together the smallest and
%% largest number in this contiguous range; in this example, these are
%% 15 and 47, producing 62.

%% What is the encryption weakness in your XMAS-encrypted list of
%% numbers?

main(_) ->
    {File, PreambleSize} =
        %% {"p9-test.txt", 5},
        {"p9.txt", 25},
    XMASEncoded = read_input(File),
    {Preamble, Rest} = lists:split(PreambleSize, XMASEncoded),
    ErrorCode = find_non_xmas(Preamble, Rest),
    EncryptionWeakness = find_weakness(ErrorCode, XMASEncoded),
    io:format("weakness: ~p~n", [EncryptionWeakness]).

find_weakness(ErrorCode, [H | T]) ->
    case find_weakness(ErrorCode-H, T, [H]) of
        'undefined' -> find_weakness(ErrorCode, T);
        Seq ->
            lists:min(Seq) + lists:max(Seq)
    end.

find_weakness(ErrorCode, [ErrorCode|_T], Seq) ->
    lists:reverse([ErrorCode | Seq]);
find_weakness(ErrorCode, [H|_T], _Seq) when H > ErrorCode ->
    'undefined';
find_weakness(_ErrorCode, [], _Seq) ->
    'undefined';
find_weakness(ErrorCode, [H|T], Seq) ->
    find_weakness(ErrorCode-H, T, [H | Seq]).

find_non_xmas([_|PNext]=Preamble, [H | T]) ->
    case is_xmas_encoded(Preamble, H) of
        'true' -> find_non_xmas(PNext ++ [H], T);
        'false' -> H
    end.

is_xmas_encoded([_], _N) -> 'false';
is_xmas_encoded([P|Preamble], N) ->
    case [Q || Q <- Preamble, N =:= Q+P] of
        [] -> is_xmas_encoded(Preamble, N);
        _Qs -> 'true'
    end.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [binary_to_integer(Line, 10) || Line <- binary:split(Lines, <<"\n">>, ['global']), Line =/= <<>>].
