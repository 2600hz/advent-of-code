#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% An Elf just remembered one more important detail: the two adjacent
%% matching digits are not part of a larger group of matching digits.

%% Given this additional criterion, but still ignoring the range rule,
%% the following are now true:

%%     112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
%%     123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
%%     111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).

%% How many different passwords within the range given in your puzzle
%% input meet all of the criteria?

%% Your puzzle input is still 206938-679128.

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    search(206938, 679128).

search(Start, End) ->
    search(Start, End, []).

search(End, End, Matches) ->
    io:format("matching passwords: ~p~n~p~n", [length(Matches), Matches]);
search(Start, End, Matches) ->
    search(Start, End, Matches, meets_criteria(Start)).

search(Start, End, Matches, 'true') ->
    search(Start+1, End, [Start | Matches]);
search(Start, End, Matches, 'false') ->
    search(Start+1, End, Matches).

meets_criteria(Password) ->
    L = integer_to_list(Password),

    has_matching_adjacent(L)
        andalso non_decreasing(L).

%% matching 6
has_matching_adjacent([N, N, N, N, N, N]) -> 'false';

%% matching 5
has_matching_adjacent([N, N, N, N, N, _]) -> 'false';
has_matching_adjacent([_, N, N, N, N, N]) -> 'false';

%% matching 4
has_matching_adjacent([N, N, N, N, M, M]) -> 'true';
has_matching_adjacent([M, M, N, N, N, N]) -> 'true';
has_matching_adjacent([_, N, N, N, N, _]) -> 'false';

%% matching 3
has_matching_adjacent([N, N, N | Rest]) ->
    has_matching_adjacent(Rest);

%% matching 2
has_matching_adjacent([N, N | _]) ->  'true';

%% matching 1
has_matching_adjacent([_N]) ->        'false';

%% matching 0
has_matching_adjacent([]) ->          'false';

%% everything else
has_matching_adjacent([_N | Rest]) -> has_matching_adjacent(Rest).

non_decreasing([]) -> 'true';
non_decreasing([_N]) -> 'true';
non_decreasing([A, B | _Rest]) when A > B -> 'false';
non_decreasing([_ | Rest]) -> non_decreasing(Rest).
