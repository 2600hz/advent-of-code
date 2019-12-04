#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Day 4: Secure Container ---

%% You arrive at the Venus fuel depot only to discover it's protected
%% by a password. The Elves had written the password on a sticky note,
%% but someone threw it out.

%% However, they do remember a few key facts about the password:

%%     It is a six-digit number.
%%     The value is within the range given in your puzzle input.
%%     Two adjacent digits are the same (like 22 in 122345).
%%     Going from left to right, the digits never decrease; they only
%%     ever increase or stay the same (like 111123 or 135679).

%% Other than the range rule, the following are true:

%%     111111 meets these criteria (double 11, never decreases).
%%     223450 does not meet these criteria (decreasing pair of digits 50).
%%     123789 does not meet these criteria (no double).

%% How many different passwords within the range given in your puzzle
%% input meet these criteria?

%% Your puzzle input is 206938-679128.

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

has_matching_adjacent([]) ->          'false';
has_matching_adjacent([_N]) ->        'false';
has_matching_adjacent([N, N | _]) ->  'true';
has_matching_adjacent([_N | Rest]) -> has_matching_adjacent(Rest).

non_decreasing([]) -> 'true';
non_decreasing([_N]) -> 'true';
non_decreasing([A, B | _Rest]) when A > B -> 'false';
non_decreasing([_ | Rest]) -> non_decreasing(Rest).
