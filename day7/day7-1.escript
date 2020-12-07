#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 7: Handy Haversacks ---

%% You land at the regional airport in time for your next flight. In
%% fact, it looks like you'll even have time to grab some food: all
%% flights are currently delayed due to issues in luggage processing.

%% Due to recent aviation regulations, many rules (your puzzle input)
%% are being enforced about bags and their contents; bags must be
%% color-coded and must contain specific quantities of other
%% color-coded bags. Apparently, nobody responsible for these
%% regulations considered how long they would take to enforce!

%% For example, consider the following rules:

%% light red bags contain 1 bright white bag, 2 muted yellow bags.
%% dark orange bags contain 3 bright white bags, 4 muted yellow bags.
%% bright white bags contain 1 shiny gold bag.
%% muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
%% shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
%% dark olive bags contain 3 faded blue bags, 4 dotted black bags.
%% vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
%% faded blue bags contain no other bags.
%% dotted black bags contain no other bags.

%% These rules specify the required contents for 9 bag types. In this
%% example, every faded blue bag is empty, every vibrant plum bag
%% contains 11 bags (5 faded blue and 6 dotted black), and so on.

%% You have a shiny gold bag. If you wanted to carry it in at least one
%% other bag, how many different bag colors would be valid for the
%% outermost bag? (In other words: how many colors can, eventually,
%% contain at least one shiny gold bag?)

%% In the above rules, the following options would be available to you:

%%     A bright white bag, which can hold your shiny gold bag directly.
%%     A muted yellow bag, which can hold your shiny gold bag directly,
%%     plus some other bags.
%%     A dark orange bag, which can hold bright white and muted yellow
%%     bags, either of which could then hold your shiny gold bag.
%%     A light red bag, which can hold bright white and muted yellow
%%     bags, either of which could then hold your shiny gold bag.

%% So, in this example, the number of bag colors that can eventually
%% contain at least one shiny gold bag is 4.

%% How many bag colors can eventually contain at least one shiny gold bag? (The list of rules is quite long; make sure you get all of it.)

-define(SHINY_GOLD, <<"shiny gold">>).

-record(bag_rule, {bag, contains_gold, contains = []}).

main(_) ->
    BagRules = read_input("p7.txt"),
    Count = count_bags_with_gold(BagRules),
    io:format("count of bags: ~p~n", [Count]).

count_bags_with_gold(BagRules) ->
    {Count, _} = lists:foldl(fun count_bag_with_gold/2
                            ,{0, BagRules}
                            ,BagRules
                            ),
    Count.

count_bag_with_gold(#bag_rule{bag=_Bag, contains_gold='false'}, Acc) ->
    Acc;
count_bag_with_gold(#bag_rule{bag=_Bag, contains_gold='true'}, {Count, Rules}) ->
    {Count+1, Rules};
count_bag_with_gold(#bag_rule{}=Rule, {Count, Rules}) ->
    case bag_contains_gold(Rule, Rules) of
        'true' ->
            {Count+1, update_rule(Rule, 'true', Rules)};
        'false' ->
            {Count, update_rule(Rule, 'false', Rules)}
    end.

update_rule(#bag_rule{bag=Bag}=Rule, HasGold, Rules) ->
    lists:keyreplace(Bag, #bag_rule.bag, Rules, Rule#bag_rule{contains_gold=HasGold}).

bag_contains_gold(#bag_rule{contains=[]}=_Rule, _Rules) -> 'false';
bag_contains_gold(#bag_rule{contains=Contains}, Rules) ->
    lists:any(fun({Q, B}) -> any_bag_contains_gold(Q, B, Rules) end, Contains).

any_bag_contains_gold(_Quantity, Bag, Rules) ->
    case lists:keyfind(Bag, #bag_rule.bag, Rules) of
        #bag_rule{bag=?SHINY_GOLD} -> 'true';
        #bag_rule{contains_gold='true'} -> 'true';
        #bag_rule{contains_gold='false'} -> 'false';
        #bag_rule{}=Rule -> bag_contains_gold(Rule, Rules)
    end.

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
