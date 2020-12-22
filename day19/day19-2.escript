#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% As you look over the list of messages, you realize your matching
%% rules aren't quite right. To fix them, completely replace rules 8:
%% 42 and 11: 42 31 with the following:

%% 8: 42 | 42 8
%% 11: 42 31 | 42 11 31

%% This small change has a big impact: now, the rules do contain loops,
%% and the list of messages they could hypothetically match is
%% infinite. You'll need to determine how these changes affect which
%% messages are valid.

%% Fortunately, many of the rules are unaffected by this change; it
%% might help to start by looking at which rules always match the same
%% set of values and how those rules (especially rules 42 and 31) are
%% used by the new versions of rules 8 and 11.

%% (Remember, you only need to handle the rules you have; building a
%% solution that could handle any hypothetical combination of rules
%% would be significantly more difficult.)

%% For example:

%% 42: 9 14 | 10 1
%% 9: 14 27 | 1 26
%% 10: 23 14 | 28 1
%% 1: "a"
%% 11: 42 31
%% 5: 1 14 | 15 1
%% 19: 14 1 | 14 14
%% 12: 24 14 | 19 1
%% 16: 15 1 | 14 14
%% 31: 14 17 | 1 13
%% 6: 14 14 | 1 14
%% 2: 1 24 | 14 4
%% 0: 8 11
%% 13: 14 3 | 1 12
%% 15: 1 | 14
%% 17: 14 2 | 1 7
%% 23: 25 1 | 22 14
%% 28: 16 1
%% 4: 1 1
%% 20: 14 14 | 1 15
%% 3: 5 14 | 16 1
%% 27: 1 6 | 14 18
%% 14: "b"
%% 21: 14 1 | 1 14
%% 25: 1 1 | 1 14
%% 22: 14 14
%% 8: 42
%% 26: 14 22 | 1 20
%% 18: 15 15
%% 7: 14 5 | 1 21
%% 24: 14 1

%% abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
%% bbabbbbaabaabba
%% babbbbaabbbbbabbbbbbaabaaabaaa
%% aaabbbbbbaaaabaababaabababbabaaabbababababaaa
%% bbbbbbbaaaabbbbaaabbabaaa
%% bbbababbbbaaaaaaaabbababaaababaabab
%% ababaaaaaabaaab
%% ababaaaaabbbaba
%% baabbaaaabbaaaababbaababb
%% abbbbabbbbaaaababbbbbbaaaababb
%% aaaaabbaabaaaaababaa
%% aaaabbaaaabbaaa
%% aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
%% babaaabbbaaabaababbaabababaaab
%% aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba

%% Without updating rules 8 and 11, these rules only match three
%% messages: bbabbbbaabaabba, ababaaaaaabaaab, and ababaaaaabbbaba.

%% However, after updating rules 8 and 11, a total of 12 messages
%% match:

%%     bbabbbbaabaabba
%%     babbbbaabbbbbabbbbbbaabaaabaaa
%%     aaabbbbbbaaaabaababaabababbabaaabbababababaaa
%%     bbbbbbbaaaabbbbaaabbabaaa
%%     bbbababbbbaaaaaaaabbababaaababaabab
%%     ababaaaaaabaaab
%%     ababaaaaabbbaba
%%     baabbaaaabbaaaababbaababb
%%     abbbbabbbbaaaababbbbbbaaaababb
%%     aaaaabbaabaaaaababaa
%%     aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
%%     aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba

%% After updating rules 8 and 11, how many messages completely match
%% rule 0?

main(_) ->
    {Rules, Messages} = read_input("p19.txt"),
    io:format("rules: ~p~n", [Rules]),
    {Matching, _} = lists:foldl(fun match_message/2, {[], Rules}, Messages),
    Matched = length(Matching),

    %% Expected = lists:usort([<<"bbabbbbaabaabba">>
    %%                        ,<<"babbbbaabbbbbabbbbbbaabaaabaaa">>
    %%                        ,<<"aaabbbbbbaaaabaababaabababbabaaabbababababaaa">>
    %%                        ,<<"bbbbbbbaaaabbbbaaabbabaaa">>
    %%                        ,<<"bbbababbbbaaaaaaaabbababaaababaabab">>
    %%                        ,<<"ababaaaaaabaaab">>
    %%                        ,<<"ababaaaaabbbaba">>
    %%                        ,<<"baabbaaaabbaaaababbaababb">>
    %%                        ,<<"abbbbabbbbaaaababbbbbbaaaababb">>
    %%                        ,<<"aaaaabbaabaaaaababaa">>
    %%                        ,<<"aaaabbaabbaaaaaaabbbabbbaaabbaabaaa">>
    %%                        ,<<"aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba">>
    %%                        ]),

    io:format("matched: ~p~n", [Matched]).

match_message(Message, {Count, Rules}) ->
    case matches_rules(Message, 0, Rules) of
        {'true', []} ->
            %% io:format("message ~s matches~n", [Message]),
            {[list_to_binary(Message) | Count], Rules};
        {'true', _M} ->
            {Count, Rules};
        'false' ->
            {Count, Rules}
    end.

matches_rules([], _, _) -> {'true', []};
matches_rules([<<M/binary>> | Message], M, _Rules) ->
    {'true', Message};
matches_rules(_Message, <<_/binary>>, _Rules) ->
    'false';
matches_rules(Message, RuleNo, Rules) ->
    matches_rule_sets(Message, Rules, maps:get(RuleNo, Rules)).

matches_rule_sets(_Message, _Rules, []) -> 'false';
matches_rule_sets([M | Message], _Rules, M) -> {'true', Message};
matches_rule_sets(_Message, _Rules, <<_/binary>>) -> 'false';

matches_rule_sets(Message, Rules, [RuleSet | RuleSets]) ->
    %% io:format("  ruleset: ~p message: ~s~n", [RuleSet, Message]),
    case matches_rule_set(Message, Rules, RuleSet) of
        {'true', M} ->
            %% io:format(" matched message part ~s with ruleset ~w~n", [M, RuleSet]),
            {'true', M};
        'false' ->
            %% io:format(" nomatch ruleset ~w~n", [RuleSet]),
            matches_rule_sets(Message, Rules, RuleSets)
    end.

matches_rule_set([], _Rules, _RuleSet) -> 'false';
matches_rule_set([M | Message], Rules, [M | RuleSet]) ->
    matches_rule_set(Message, Rules, RuleSet);
matches_rule_set(Message, Rules, RuleSet) ->
    lists:foldl(fun(RuleNo, {'true', M}) ->
                        %% io:format(" fold rule ~p of ~p on ~s~n", [RuleNo, RuleSet, M]),
                        matches_rules(M, RuleNo, Rules);
                   (_RuleNo, 'false') -> 'false'
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
    {resolve_rules(Rules), [ [<<M>> || <<M>> <= Message] || Message <- Messages]};
read_rules([Rule | Rules], ParsedRules) ->
    read_rules(Rules, parse_rule(Rule, ParsedRules)).

resolve_rules(Rules) ->
    maps:fold(fun resolve_rule/3, Rules, Rules).

resolve_rule(_RuleNo, <<_/binary>>, Rules) ->
    Rules;
resolve_rule(RuleNo, RuleSets, Rules) ->
    Rules#{RuleNo => lists:foldl(fun(RS, Acc) -> resolve_rule_set(RS, Acc, Rules) end, [], RuleSets)}.

resolve_rule_set(RuleSet, Acc0, Rules) ->
    lists:foldr(fun(<<_/binary>>=RuleNo, Acc) -> [RuleNo, Acc];
                   (RuleNo, Acc) ->
                        case maps:get(RuleNo, Rules) of
                            <<C/binary>> -> [C | Acc];
                            _R -> [RuleNo | Acc]
                        end
                end
               ,Acc0
               ,RuleSet
               ).

parse_rule(Rule, ParsedRules) ->
    {'match', [RuleNo, RuleSet]} = re:run(Rule, <<"(\\d+): (.+)">>, [{'capture', 'all_but_first', 'binary'}]),

    maybe_override_rule(ParsedRules, binary_to_integer(RuleNo, 10), parse_rule_set(RuleSet)).

maybe_override_rule(ParsedRules, 8, _Rule) ->
    ParsedRules#{8 => [[42], [42, 8]]};
maybe_override_rule(ParsedRules, 11, _Rule) ->
    ParsedRules#{11 => [[42, 31], [42, 11, 31]]};
maybe_override_rule(ParsedRules, RuleNo, <<Rule/binary>>) ->
    maps:fold(fun(_RNo, <<_/binary>>, PR) -> PR;
                 (RNo, Set, PR) ->
                      PR#{RNo => lists:foldr(fun(R, S) when R=:=RuleNo -> [Rule | S]; (N, S) -> [N | S] end, [], Set)}
              end
             ,ParsedRules#{RuleNo => Rule}
             ,ParsedRules
             );
maybe_override_rule(ParsedRules, RuleNo, RuleSets) ->
    Resolved = [lists:foldr(fun(RNo, Acc) ->
                                    case maps:get(RNo, ParsedRules, 'undefined') of
                                        <<C/binary>> -> [C | Acc];
                                        _ -> [RNo | Acc]
                                    end
                            end
                           ,[]
                           ,RuleSet
                           )
                || RuleSet <- RuleSets
               ],
    ParsedRules#{RuleNo => Resolved}.

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
