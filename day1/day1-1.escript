#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% Before you leave, the Elves in accounting just need you to fix your
%% expense report (your puzzle input); apparently, something isn't
%% quite adding up.

%% Specifically, they need you to find the two entries that sum to 2020
%% and then multiply those two numbers together.

%% For example, suppose your expense report contained the following:

%% 1721
%% 979
%% 366
%% 299
%% 675
%% 1456

%% In this list, the two entries that sum to 2020 are 1721 and
%% 299. Multiplying them together produces 1721 * 299 = 514579, so the
%% correct answer is 514579.

%% Of course, your expense report is much larger. Find the two entries
%% that sum to 2020; what do you get if you multiply them together?

main(_) ->
    [H|Input] = read_input("p1.txt"),
    {X, Y} = find_2020(Input, H),
    io:format("answer: ~p~n", [X*Y]).

find_2020(Input, H) ->
    case foldl(Input, H) of
        {X, Y} -> {X, Y};
        'undefined' -> find_2020(tl(Input), hd(Input))
    end.

foldl([], _) -> 'undefined';
foldl([Y|_], X) when X+Y =:= 2020 -> {X, Y};
foldl([_|T], X) -> foldl(T, X).

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [binary_to_integer(Line, 10) || Line <- binary:split(Lines, <<"\n">>, ['global']), Line =/= <<>>].
