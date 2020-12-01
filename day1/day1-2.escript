#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% The Elves in accounting are thankful for your help; one of them even
%% offers you a starfish coin they had left over from a past
%% vacation. They offer you a second one if you can find three numbers
%% in your expense report that meet the same criteria.

%% Using the above example again, the three entries that sum to 2020
%% are 979, 366, and 675. Multiplying them together produces the
%% answer, 241861950.

%% In your expense report, what is the product of the three entries
%% that sum to 2020?

main(_) ->
    Entries = read_input("p1.txt"),
    {X, Y, Z} = find_triple(Entries),
    io:format("answer: ~p: ~p~n", [{X, Y, Z}, X * Y * Z]).

find_triple([X | Entries]) ->
    case find_double(2020-X, Entries) of
        'undefined' -> find_triple(Entries);
        {Y, Z} -> {X, Y, Z}
    end.

find_double(_Total, []) -> 'undefined';
find_double(Total, _) when Total < 0 -> 'undefined';
find_double(Total, [Y | Entries]) when Y >= Total ->
    find_double(Total, Entries);
find_double(Total, [Y | Entries]) ->
    case [Z || Z <- Entries, Y+Z =:= Total] of
        [] -> find_double(Total, Entries);
        [Z] -> {Y, Z}
    end.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    lists:foldl(fun read_input_line/2, [], binary:split(Lines, <<"\n">>, ['global'])).

read_input_line(<<>>, Acc) -> Acc;
read_input_line(Line, Acc) ->
    [binary_to_integer(Line, 10) | Acc].
