#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 14: Extended Polymerization ---

%% The incredible pressures at this depth are starting to put a strain
%% on your submarine. The submarine has polymerization equipment that
%% would produce suitable materials to reinforce the submarine, and
%% the nearby volcanically-active caves should even have the necessary
%% input elements in sufficient quantities.

%% The submarine manual contains instructions for finding the optimal
%% polymer formula; specifically, it offers a polymer template and a
%% list of pair insertion rules (your puzzle input). You just need to
%% work out what polymer would result after repeating the pair
%% insertion process a few times.

%% For example:

%% NNCB

%% CH -> B
%% HH -> N
%% CB -> H
%% NH -> C
%% HB -> C
%% HC -> B
%% HN -> C
%% NN -> C
%% BH -> H
%% NC -> B
%% NB -> B
%% BN -> B
%% BB -> N
%% BC -> B
%% CC -> N
%% CN -> C

%% The first line is the polymer template - this is the starting point
%% of the process.

%% The following section defines the pair insertion rules. A rule like
%% AB -> C means that when elements A and B are immediately adjacent,
%% element C should be inserted between them. These insertions all
%% happen simultaneously.

%% So, starting with the polymer template NNCB, the first step
%% simultaneously considers all three pairs:

%%     The first pair (NN) matches the rule NN -> C, so element C is
%%     inserted between the first N and the second N.

%%     The second pair (NC) matches the rule NC -> B, so element B is
%%     inserted between the N and the C.

%%     The third pair (CB) matches the rule CB -> H, so element H is
%%     inserted between the C and the B.

%% Note that these pairs overlap: the second element of one pair is
%% the first element of the next pair. Also, because all pairs are
%% considered simultaneously, inserted elements are not considered to
%% be part of a pair until the next step.

%% After the first step of this process, the polymer becomes NCNBCHB.

%% Here are the results of a few steps using the above rules:

%% Template:     NNCB
%% After step 1: NCNBCHB
%% After step 2: NBCCNBBBCBHCB
%% After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
%% After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

%% This polymer grows quickly. After step 5, it has length 97; After
%% step 10, it has length 3073. After step 10, B occurs 1749 times, C
%% occurs 298 times, H occurs 161 times, and N occurs 865 times;
%% taking the quantity of the most common element (B, 1749) and
%% subtracting the quantity of the least common element (H, 161)
%% produces 1749 - 161 = 1588.

%% Apply 10 steps of pair insertion to the polymer template and find
%% the most and least common elements in the result. What do you get
%% if you take the quantity of the most common element and subtract
%% the quantity of the least common element?

%% --- Part Two ---

%% The resulting polymer isn't nearly strong enough to reinforce the
%% submarine. You'll need to run more steps of the pair insertion
%% process; a total of 40 steps should do it.

%% In the above example, the most common element is B (occurring
%% 2192039569602 times) and the least common element is H (occurring
%% 3849876073 times); subtracting these produces 2188189693529.

%% Apply 40 steps of pair insertion to the polymer template and find
%% the most and least common elements in the result. What do you get
%% if you take the quantity of the most common element and subtract
%% the quantity of the least common element?

main(_) ->
    Input = read_input("p14-test.txt"),
    p14_1(Input),
    p14_2(Input).

p14_1({Template, Insertions}) ->
    Steps = 1,
    Polymer = steps(Template, Insertions, Steps),
    io:format("polymer: ~p~n", [Polymer]),
    {{MinEl, Min}
    ,{MaxEl, Max}
    } = tally_elements(Polymer),
    io:format("after ~p: ~p(~s) - ~p(~s) = ~p~n", [Steps, Max, MaxEl, Min, MinEl, Max-Min]).

tally_elements(Polymer) ->
    Tally = maps:fold(fun tally_element/3, #{}, Polymer),
    io:format("tally: ~p~n", [Tally]),
    [{MinEl, Min} | Rest] = lists:keysort(2, maps:to_list(Tally)),
    [{MaxEl, Max} | _] = lists:reverse(Rest),
    {{[MinEl], Min}, {[MaxEl], Max}}.

tally_element($0, Last, Tally) ->
    tally_point(Last, maps:get(Last, Tally, 0), Tally);
tally_element([A, B], N, Tally) ->
    %% tally_point(B, N, Tally).
    tally_point(A, N, Tally).
%% ).

tally_point(A, N, Tally) ->
    IncrFun = fun(V) -> io:format("~p: tally ~p~n", [A, V]), V+1 end,
    maps:update_with(A, IncrFun, N, Tally).

incr_1(N) -> N+1.

steps(<<Template/binary>>, Insertions, Steps) ->
    steps(init_polymer(Template), Insertions, Steps);
steps(Polymer, _Insertions, 0) ->
    io:format("done~n"),
    Polymer;
steps(Polymer, Insertions, StepsLeft) ->
    io:format("step ~b~n", [StepsLeft]),
    steps(step(Polymer, Insertions), Insertions, StepsLeft-1).

init_polymer(Template) ->
    init_polymer(binary_to_list(Template), #{}).

init_polymer([Last], Polymer) ->
    io:format("init polymer: ~p~n", [Polymer]),
    Polymer#{$0 => Last};
init_polymer([A, B | Rest], Polymer) ->
    init_polymer([B | Rest], Polymer#{[A, B] => maps:get([A, B], Polymer, 0) + 1}).

step(Polymer, Insertions) ->
    {NewPolymer, _} = maps:fold(fun step_pair/3, {#{}, Insertions}, Polymer),

    Tally = maps:fold(fun tally_element/3, #{}, Polymer),
    io:format(" running tally: ~p~n", [Tally]),

    NewPolymer.

step_pair($0, _Last, {Polymer, Insertions}) ->
    {Polymer, Insertions};
step_pair([A, B], V, {Polymer, Insertions}) ->
    C = maps:get([A, B], Insertions),
    io:format("pair ~s (~w) => ~s~n", [[A, B], V, [C]]),
    {incr_pair(incr_pair(Polymer, [A, C], V), [C, B], V)
    ,Insertions
    }.

incr_pair(Polymer, Pair, V) ->
    io:format("  incr p-pair: ~p ~p+~p~n", [Pair, V, maps:get(Pair, Polymer, 0)]),
    maps:update_with(Pair, fun(N) -> N+V end, V+1, Polymer).

p14_2({Template, Insertions}) ->
    {Template, Insertions}.
%% Steps = 40,
%% Polymer = steps(Template, Insertions, Steps),
%% {{MinEl, Min}
%% ,{MaxEl, Max}
%% } = tally_elements(Polymer),
%% io:format("after ~p: ~p(~s) - ~p(~s) = ~p~n", [Steps, Max, MaxEl, Min, MinEl, Max-Min]).

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    read_template(binary:split(Lines, <<"\n">>, ['global'])).

read_template([Template, <<>> | PairInsertions]) ->
    {Template, read_insertions(PairInsertions, #{})}.

read_insertions([], Insertions) -> Insertions;
read_insertions([<<>>|Pairs], Insertions) -> read_insertions(Pairs, Insertions);
read_insertions([<<A:8, B:8, " -> ", C:8>> | Pairs], Insertions) ->
    read_insertions(Pairs, Insertions#{[A, B] => C}).
