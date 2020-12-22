#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 19: Monster Messages ---

%% You land in an airport surrounded by dense forest. As you walk to
%% your high-speed train, the Elves at the Mythical Information Bureau
%% contact you again. They think their satellite has collected an image
%% of a sea monster! Unfortunately, the connection to the satellite is
%% having problems, and many of the messages sent back from the
%% satellite have been corrupted.

%% They sent you a list of the rules valid messages should obey and a
%% list of received messages they've collected so far (your puzzle
%% input).

%% The rules for valid messages (the top part of your puzzle input) are
%% numbered and build upon each other. For example:

%% 0: 1 2
%% 1: "a"
%% 2: 1 3 | 3 1
%% 3: "b"

%% Some rules, like 3: "b", simply match a single character (in this
%% case, b).

%% The remaining rules list the sub-rules that must be followed; for
%% example, the rule 0: 1 2 means that to match rule 0, the text being
%% checked must match rule 1, and the text after the part that matched
%% rule 1 must then match rule 2.

%% Some of the rules have multiple lists of sub-rules separated by a
%% pipe (|). This means that at least one list of sub-rules must
%% match. (The ones that match might be different each time the rule is
%% encountered.) For example, the rule 2: 1 3 | 3 1 means that to match
%% rule 2, the text being checked must match rule 1 followed by rule 3
%% or it must match rule 3 followed by rule 1.

%% Fortunately, there are no loops in the rules, so the list of
%% possible matches will be finite. Since rule 1 matches a and rule 3
%% matches b, rule 2 matches either ab or ba. Therefore, rule 0 matches
%% aab or aba.

%% Here's a more interesting example:

%% 0: 4 1 5
%% 1: 2 3 | 3 2
%% 2: 4 4 | 5 5
%% 3: 4 5 | 5 4
%% 4: "a"
%% 5: "b"

%% Here, because rule 4 matches a and rule 5 matches b, rule 2 matches
%% two letters that are the same (aa or bb), and rule 3 matches two
%% letters that are different (ab or ba).

%% Since rule 1 matches rules 2 and 3 once each in either order, it
%% must match two pairs of letters, one pair with matching letters and
%% one pair with different letters. This leaves eight possibilities:
%% aaab, aaba, bbab, bbba, abaa, abbb, baaa, or babb.

%% Rule 0, therefore, matches a (rule 4), then any of the eight options
%% from rule 1, then b (rule 5): aaaabb, aaabab, abbabb, abbbab,
%% aabaab, aabbbb, abaaab, or ababbb.

%% The received messages (the bottom part of your puzzle input) need to
%% be checked against the rules so you can determine which are valid
%% and which are corrupted. Including the rules and the messages
%% together, this might look like:

%% 0: 4 1 5
%% 1: 2 3 | 3 2
%% 2: 4 4 | 5 5
%% 3: 4 5 | 5 4
%% 4: "a"
%% 5: "b"

%% ababbb
%% bababa
%% abbbab
%% aaabbb
%% aaaabbb

%% Your goal is to determine the number of messages that completely
%% match rule 0. In the above example, ababbb and abbbab match, but
%% bababa, aaabbb, and aaaabbb do not, producing the answer 2. The
%% whole message must match all of rule 0; there can't be extra
%% unmatched characters in the message. (For example, aaaabbb might
%% appear to match rule 0 above, but it has an extra unmatched b on the
%% end.)

%% How many messages completely match rule 0?

main(_) ->
    {Rules, Messages} = read_input("p19.txt"),
    {Matched, _} = lists:foldl(fun match_message/2, {0, Rules}, Messages),
    io:format("matched: ~p~n", [Matched]).

match_message(Message, {Count, Rules}) ->
    case matches_rules(Message, 0, Rules) of
        {'true', []} ->
            %% io:format("message ~s matches~n", [Message]),
            {Count+1, Rules};
        {'true', _M} ->
            {Count, Rules};
        'false' ->
            {Count, Rules}
    end.

matches_rules([], _, _) -> {'true', []};
matches_rules([M | Message], RuleNo, Rules) ->
    case maps:get(RuleNo, Rules) of
        M ->
            %% io:format(" matched message part ~p(~p)~n", [RuleNo, M]),
            {'true', Message};
        <<_C/binary>> ->
            %% io:format(" nomatch ~p(~p)~n", [RuleNo, _C]),
            'false';
        RuleSets ->
            %% io:format(" ruleset ~p: ~p~n", [RuleNo, RuleSets]),
            matches_rule_sets([M | Message], Rules, RuleSets)
    end.

matches_rule_sets(_Message, _Rules, []) ->
    %% io:format("  out of rulesets for message: ~p~n", [Message]),
    'false';
matches_rule_sets(Message, Rules, [RuleSet | RuleSets]) ->
    case matches_rule_set(Message, Rules, RuleSet) of
        {'true', M} ->
            %% io:format(" matched message ~p with ~p~n", [Message, RuleSet]),
            {'true', M};
        'false' ->
            %% io:format(" nomatch ruleset ~p~n", [RuleSet]),
            matches_rule_sets(Message, Rules, RuleSets)
    end.

matches_rule_set(Message, Rules, RuleSet) ->
    lists:foldl(fun(RuleNo, {'true', M}) ->
                        %% io:format(" fold rule ~p of ~p on ~s~n", [RuleNo, RuleSet, M]),
                        matches_rules(M, RuleNo, Rules);
                   (_, 'false') -> 'false'
                end
               ,{'true', Message}
               ,RuleSet
               ).

read_input(Filename) ->
    {'ok', File} = file:read_file(Filename),
    read_rules(binary:split(File, <<"\n">>, ['global', 'trim'])).

read_rules(Lines) ->
    read_rules(Lines, #{}).

read_rules([<<>> | Messages], Rules) ->
    {Rules, [ [<<M>> || <<M>> <= Message] || Message <- Messages]};
read_rules([Rule | Rules], ParsedRules) ->
    read_rules(Rules, parse_rule(Rule, ParsedRules)).

parse_rule(Rule, ParsedRules) ->
    {'match', [RuleNo, RuleSet]} = re:run(Rule, <<"(\\d+): (.+)">>, [{'capture', 'all_but_first', 'binary'}]),

    ParsedRules#{(binary_to_integer(RuleNo, 10)) => parse_rule_set(RuleSet)}.

parse_rule_set(<<$", Char:1/binary, $">>) ->
    Char;
parse_rule_set(RuleSet) ->
    Groups = binary:split(RuleSet, <<" | ">>, ['global']),
    lists:foldl(fun parse_group/2, [], Groups).

parse_group(Group, Groups) ->
    [[binary_to_integer(G, 10) ||
         G <-binary:split(Group, <<" ">>, ['global'])
     ]
    | Groups
    ].
