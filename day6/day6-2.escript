#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% As you finish the last group's customs declaration, you notice that
%% you misread one word in the instructions:

%% You don't need to identify the questions to which anyone answered
%% "yes"; you need to identify the questions to which everyone answered
%% "yes"!

%% Using the same example as above:

%% abc

%% a
%% b
%% c

%% ab
%% ac

%% a
%% a
%% a
%% a

%% b

%% This list represents answers from five groups:

%%     In the first group, everyone (all 1 person) answered "yes" to 3
%%     questions: a, b, and c.
%%     In the second group, there is no question to which everyone
%%     answered "yes".
%%     In the third group, everyone answered yes to only 1 question,
%%     a. Since some people did not answer "yes" to b or c, they don't
%%     count.
%%     In the fourth group, everyone answered yes to only 1 question,
%%     a.
%%     In the fifth group, everyone (all 1 person) answered "yes" to 1
%%     question, b.

%% In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6.

%% For each group, count the number of questions to which everyone
%% answered "yes". What is the sum of those counts?


main(_) ->
    Groups = read_input("p6.txt"),
    Sum = lists:foldl(fun count_group_answers/2, 0, Groups),
    io:format("sum: ~p~n", [Sum]).

count_group_answers([Member | Members], Sum) ->
    SameSame = lists:foldl(fun intersection/2, Member, Members),
    sets:size(SameSame) + Sum.

intersection(Member, Intersection) ->
    sets:intersection(Member, Intersection).

read_input(File) ->
    {'ok', Answers} = file:read_file(File),
    case lists:foldl(fun split_into_groups/2, {[], []}, binary:split(Answers, <<"\n">>, ['global'])) of
        {[], Groups} -> Groups;
        {Group, Groups} -> [Group | Groups]
    end.

split_into_groups(<<>>, {Group, Groups}) ->
    {[], [Group | Groups]};
split_into_groups(Line, {Group, Groups}) ->
    Answers = sets:from_list([<<C>> || <<C>> <= Line]),
    {[Answers | Group], Groups}.
