-module(day7).

-export([total_winnings/2]).

total_winnings(Input, Part) ->
    Hands = parse_hands(Input),
    HandsWithSort = [add_sorted_hand(Hand, Part) || Hand <- Hands],
    HandsWithTypes = [add_hand_type(Hand) || Hand <- HandsWithSort],
    SortedHands = lists:sort(type_comparer(Part), HandsWithTypes),
    EnumeratedHands = lists:enumerate(SortedHands),
    lists:sum([Bid * Rank || {Rank, {_, _, Bid, _}} <- EnumeratedHands]).

add_sorted_hand({Hand, Bid}, Part) ->
    {Hand, sorted_hand(Hand, Part), Bid}.

add_hand_type({Hand, SortedHand, Bid}) ->
    {Hand, SortedHand, Bid, hand_type(SortedHand)}.

sorted_hand(Hand, Part) ->
    OccurrencesMap = lists:foldl(fun add_occurrence/2, maps:new(), Hand),
    Occurrences = maps:to_list(OccurrencesMap),
    Sorted = case Part of
                 1 -> lists:keysort(2, Occurrences);
                 2 -> lists:sort(fun compare_occurrences/2, Occurrences)
             end,
    occurrences_to_hand(Sorted, Part).

add_occurrence(C, Map) ->
    maps:update_with(C, fun(N) -> N + 1 end, 1, Map).

compare_occurrences({$J, _N1}, {_C2, _N2}) -> 'false';
compare_occurrences({_C1, _N1}, {$J, _N2}) -> 'true';
compare_occurrences({_C1, N1}, {_C2, N2}) -> N1 < N2.

occurrences_to_hand(Occurrences, 1) ->
    lists:flatmap(fun expand_occurrences/1, Occurrences);
occurrences_to_hand(Occurrences, 2) ->
    Hand = occurrences_to_hand(Occurrences, 1),
    convert_jokers(Hand).

expand_occurrences({C, N}) -> lists:duplicate(N, C).

convert_jokers(Hand) -> convert_jokers(Hand, []).

convert_jokers([], Acc) -> lists:reverse(Acc);
convert_jokers([C1, $J | Hand], Acc) ->
    convert_jokers([C1 | Hand], [C1 | Acc]);
convert_jokers([C | Hand], Acc) ->
    convert_jokers(Hand, [C | Acc]).

hand_type([C, C, C, C, C]) -> {7, 'five_of_a_kind'};
hand_type([_, C, C, C, C]) -> {6, 'four_of_a_kind'};
hand_type([C1, C1, C2, C2, C2]) -> {5, 'full_house'};
hand_type([_, _, C, C, C]) -> {4, 'three_of_a_kind'};
hand_type([_, C1, C1, C2, C2]) -> {3, 'two_pair'};
hand_type([_, _, _, C, C]) -> {2, 'one_pair'};
hand_type(_) -> {1, 'high_card'}.

type_comparer(Part) ->
    fun(Hand1, Hand2) ->
        compare_types(Hand1, Hand2, Part)
    end.

compare_types({Hand1, _, _, {Type, _}}, {Hand2, _, _, {Type, _}}, Part) ->
    compare_cards(Hand1, Hand2, Part);
compare_types({_, _, _, {Type1, _}}, {_, _, _, {Type2, _}}, _Part) ->
    Type1 < Type2.

compare_cards([], [], _Part) -> 'true';
compare_cards([C1 | Hand1], [C2 | Hand2], Part) ->
    CS1 = card_strength(C1, Part),
    CS2 = card_strength(C2, Part),
    if CS1 > CS2 -> 'false';
       CS1 < CS2 -> 'true';
       CS1 =:= CS2 -> compare_cards(Hand1, Hand2, Part)
    end.

card_strength($A, _Part) -> 14;
card_strength($K, _Part) -> 13;
card_strength($Q, _Part) -> 12;
card_strength($J, 1) -> 11;
card_strength($J, 2) -> 1;
card_strength($T, _Part) -> 10;
card_strength(C, _Part) -> C - $0.

parse_hands(Input) ->
    Hands = binary:split(Input, <<"\n">>, ['global']),
    [parse_hand(Hand) || Hand <- Hands].

parse_hand(<<Hand:5/binary, " ", Bid/binary>>) ->
    {binary_to_list(Hand), binary_to_integer(Bid)}.
