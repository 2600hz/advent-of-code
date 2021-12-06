#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

main(_) ->
    Input = read_input("p6-test.txt"),
    p6_1(Input),
    p6_2(Input).

p6_1(Input) ->
    Input.

p6_2(Input) ->
    Input.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [binary_to_integer(Line, 10) || Line <- binary:split(Lines, <<"\n">>, ['global']), Line =/= <<>>].
