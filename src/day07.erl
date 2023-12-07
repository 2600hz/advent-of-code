-module(day07).

-export([run/0]).

%% https://adventofcode.com/2023/day/7
%% total winnings: 250120186

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

hand_strengths(Hands) ->
    [{hand_strength(Hand), Hand, Bid} || {Hand, Bid} <- Hands].

part2(Input) ->
    Input.

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.

card_strength($A) -> 14;
card_strength($K) ->  13;
card_strength($Q) ->  12;
card_strength($J) ->  11;
card_strength($T) -> 10;
card_strength(N) ->  N-$0. % 2-9

hand_strength(<<Hand/binary>>) ->
    hand_strength(rank_hand(Hand));
hand_strength('five') -> 6;
hand_strength('four') -> 5;
hand_strength('full') -> 4;
hand_strength('three') -> 3;
hand_strength('two') -> 2;
hand_strength('one') -> 1;
hand_strength('high') -> 0.

rank_hand(Hand) ->
    Cards = [Card || <<Card>> <= Hand],
    rank_cards(lists:sort(Cards)).

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
