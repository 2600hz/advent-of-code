-module(day07).

-export([run/0]).

%% https://adventofcode.com/2023/day/7
%% total winnings: 250120186
%% total joker winnings: 250665248

run() ->
    Hands = parse_hands(input("day07.txt")),
    part1(Hands),
    part2(Hands).

parse_hands(Input) ->
    HandsBins = binary:split(Input, <<$\n>>, ['global', 'trim']),
    [parse_hand(HandBin) || HandBin <- HandsBins].

parse_hand(HandBin) ->
    [Hand, BidBin] = binary:split(HandBin, <<" ">>),
    {Hand, binary_to_integer(BidBin)}.

part1(Hands) ->
    HandStrengths = hand_strengths(Hands),
    Sorted = lists:usort(fun rank_hands/2, HandStrengths),
    TotalWinnings = total_winnings(Sorted),
    io:format("total winnings: ~p~n", [TotalWinnings]).

total_winnings(RankedHands) ->
    {_Multiplier, Total} = lists:foldl(fun winnings/2, {1, 0}, RankedHands),
    Total.

winnings({_Rank, _Hand, Bid}, {Multiplier, Total}) ->
    {Multiplier+1, Total + (Bid * Multiplier)}.

rank_hands({Rank, HandA, _}, {Rank, HandB, _}) ->
    compare_hands(HandA, HandB);
rank_hands({RankA, _, _}, {RankB, _, _}) ->
    RankA < RankB.

compare_hands(<<A:8, As/binary>>, <<A:8, Bs/binary>>) ->
    compare_hands(As, Bs);
compare_hands(<<A:8, _/binary>>, <<B:8, _/binary>>) ->
    card_strength(A) < card_strength(B).

rank_joker_hands({Rank, HandA, _}, {Rank, HandB, _}) ->
    compare_joker_hands(HandA, HandB);
rank_joker_hands({RankA, _, _}, {RankB, _, _}) ->
    RankA < RankB.

compare_joker_hands(<<A/binary>>, <<A/binary>>) -> 'true';
compare_joker_hands(<<A:8, As/binary>>, <<A:8, Bs/binary>>) ->
    compare_joker_hands(As, Bs);
compare_joker_hands(<<A:8, _/binary>>, <<B:8, _/binary>>) ->
    joker_strength(A) < joker_strength(B).

hand_strengths(Hands) ->
    [{hand_strength(Hand), Hand, Bid} || {Hand, Bid} <- Hands].

part2(Hands) ->
    HandStrengths = hand_strengths_jokers(Hands),
    Sorted = lists:usort(fun rank_joker_hands/2, HandStrengths),
    TotalWinnings = total_winnings(Sorted),
    io:format("total joker winnings: ~p~n", [TotalWinnings]).

hand_strengths_jokers(Hands) ->
    [hand_strength_joker(Hand, Bid) || {Hand, Bid} <- Hands].

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.

card_strength($A) -> 14;
card_strength($K) ->  13;
card_strength($Q) ->  12;
card_strength($J) ->  11;
card_strength($T) -> 10;
card_strength(N) ->  N-$0. % 2-9

joker_strength($J) -> 1;
joker_strength(N) -> card_strength(N).

hand_strength(<<Hand/binary>>) ->
    hand_strength(rank_hand(Hand));
hand_strength('five') -> 6;
hand_strength('four') -> 5;
hand_strength('full') -> 4;
hand_strength('three') -> 3;
hand_strength('two') -> 2;
hand_strength('one') -> 1;
hand_strength('high') -> 0.

hand_strength_joker(<<Hand/binary>>, Bid) ->
    rank_hand_joker(Hand, Bid).

rank_hand(Hand) ->
    Cards = [Card || <<Card>> <= Hand],
    rank_cards(lists:sort(Cards)).

rank_hand_joker(HandBin, Bid) ->
    NonJokers = [Card || <<Card>> <= HandBin, Card =/= $J],

    case 5 - length(NonJokers)  of
        0 -> {hand_strength(rank_cards(lists:sort(NonJokers))), HandBin, Bid};
        5 -> {hand_strength('five'), HandBin, Bid};
        GroupSize ->
            Additions = permutations(NonJokers, GroupSize),

            RankedHands = [{hand_strength(list_to_binary(NonJokers ++ Addition))
                           ,list_to_binary((NonJokers ++ Addition))
                           ,Bid
                           }
                           || Addition <- Additions
                          ],
            Sorted = lists:usort(fun rank_joker_hands/2, RankedHands),
            {Rank, _BestHand, Bid} = hd(lists:reverse(Sorted)),
            {Rank, HandBin, Bid}
    end.

permutations(List, Size) ->
    lists:foldl(fun(C, Dupes) ->
                        [lists:duplicate(Size, C) | Dupes]
                end
               ,lists:filter(fun(P) -> length(P) =:= Size end, perms(lists:usort(List)))
               ,List
               ).

perms([]) -> [[]];
perms(L) ->
    [ [H|T] || H <- L, T <- perms(L--[H]) ].

rank_cards([A, A, A, A, A]) -> 'five';
rank_cards([A, A, A, A, _]) -> 'four';
rank_cards([_, A, A, A, A]) -> 'four';
rank_cards([A, A, A, B, B]) -> 'full';
rank_cards([A, A, B, B, B]) -> 'full';
rank_cards([A, A, A, _, _]) -> 'three';
rank_cards([_, A, A, A, _]) -> 'three';
rank_cards([_, _, A, A, A]) -> 'three';
rank_cards([A, A, B, B, _]) -> 'two';
rank_cards([_, A, A, B, B]) -> 'two';
rank_cards([A, A, _, B, B]) -> 'two';
rank_cards([A, A, _, _, _]) -> 'one';
rank_cards([_, A, A, _, _]) -> 'one';
rank_cards([_, _, A, A, _]) -> 'one';
rank_cards([_, _, _, A, A]) -> 'one';
rank_cards(_) -> 'high'.
