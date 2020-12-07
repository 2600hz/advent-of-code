#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% It's getting pretty expensive to fly these days - not because of
%% ticket prices, but because of the ridiculous number of bags you need
%% to buy!

%% Consider again your shiny gold bag and the rules from the above
%% example:

%%     faded blue bags contain 0 other bags.
%%     dotted black bags contain 0 other bags.
%%     vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
%%     dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.

%% So, a single shiny gold bag must contain 1 dark olive bag (and the 7
%% bags within it) plus 2 vibrant plum bags (and the 11 bags within
%% each of those): 1 + 1*7 + 2 + 2*11 = 32 bags!

%% Of course, the actual rules have a small chance of going several
%% levels deeper than this example; be sure to count all of the bags,
%% even if the nesting becomes topologically impractical!

%% Here's another example:

%% shiny gold bags contain 2 dark red bags.
%% dark red bags contain 2 dark orange bags.
%% dark orange bags contain 2 dark yellow bags.
%% dark yellow bags contain 2 dark green bags.
%% dark green bags contain 2 dark blue bags.
%% dark blue bags contain 2 dark violet bags.
%% dark violet bags contain no other bags.

%% In this example, a single shiny gold bag must contain 126 other bags.

%% How many individual bags are required inside your single shiny gold bag?

-define(SHINY_GOLD, <<"shiny gold">>).

-record(bag_rule, {bag, contains_gold, contains = []}).

main(_) ->
    BagRules = read_input("p7.txt"),
    Count = count_bags_needed(BagRules),
    io:format("count of bags: ~p~n", [Count]).

count_bags_needed(BagRules) ->
    #bag_rule{contains=Contains} = get_rule(?SHINY_GOLD, BagRules),
    SubCounts = count_bags_contained(BagRules, Contains),
    lists:sum(SubCounts) .

count_bags_contained(BagRules, Contains) ->
    [count_bag_contained(BagRules, Contain) || Contain <- Contains].

count_bag_contained(BagRules, {Quantity, Bag}) ->
    case get_rule(Bag, BagRules) of
        #bag_rule{contains=[]} -> Quantity;
        #bag_rule{contains=Contains} ->
            SubCounts = count_bags_contained(BagRules, Contains),
            (lists:sum(SubCounts) * Quantity) + Quantity
    end.

get_rule(Bag, BagRules) ->
    lists:keyfind(Bag, #bag_rule.bag, BagRules).

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    parse_rules(Lines).

parse_rules(Lines) ->
    lists:foldl(fun parse_rule/2, [], binary:split(Lines, <<"\n">>, ['global'])).

parse_rule(<<>>, Rules) -> Rules;
parse_rule(Line, Rules) ->
    case binary:split(Line, <<" bags contain ">>) of
        [?SHINY_GOLD, Rest] ->
            Contains = parse_contains(Rest),
            [#bag_rule{bag=?SHINY_GOLD, contains_gold='false', contains=Contains} | Rules];
        [BagDescription, Rest] ->
            Contains = parse_contains(Rest),
            [#bag_rule{bag=BagDescription, contains_gold='undefined', contains=Contains} | Rules]
    end.

parse_contains(<<"no ", _/binary>>) -> [];
parse_contains(Rest) ->
    {'match', Matches} = re:run(Rest, <<"(\\d+) (\\w+ \\w+) bag">>, [{'capture', 'all_but_first', 'binary'}, 'global']),
    lists:foldl(fun parse_contains_match/2, [], Matches).

parse_contains_match([Quantity, BagDescription], Contains) ->
    [{binary_to_integer(Quantity, 10), BagDescription} | Contains].
