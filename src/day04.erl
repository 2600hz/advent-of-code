-module(day04).

-export([run/0]).

%% https://adventofcode.com/2023/day/4

%% winning cards are worth: 26218
%% total scratchcards: 9997537

run() ->
    Cards = parse_cards(input("day04.txt")),
    part1(Cards),
    part2(Cards).

part1(Cards) ->
    Sum = score_cards(Cards),
    io:format("winning cards are worth: ~p~n", [Sum]).

score_cards(Cards) ->
    maps:fold(fun score_cards/3, 0, Cards).

score_cards(_CardNo, Numbers, Sum) ->
    Sum + score_card(Numbers).

score_card(Numbers) ->
    case matching_numbers(Numbers) of
        0 -> 0;
        Size -> round(math:pow(2, Size-1))
    end.

matching_numbers({Winning, Have}) ->
    Set = sets:intersection(sets:from_list(Winning)
                           ,sets:from_list(Have)
                           ),
    sets:size(Set).

parse_cards(Input) ->
    Cards = binary:split(Input, <<$\n>>, ['global', 'trim']),
    lists:foldl(fun parse_card/2, #{}, Cards).

parse_card(CardBin, Cards) ->
    [<<"Card ", CardNo/binary>>, NumbersBin] = binary:split(CardBin, <<$:>>),
    [WinningBin, HaveBin] = binary:split(NumbersBin, <<$|>>, ['trim']),
    WinningNumbers = to_numbers(WinningBin),
    HaveNumbers = to_numbers(HaveBin),
    CardNumber = binary_to_integer(string:trim(CardNo)),
    Cards#{CardNumber => {WinningNumbers, HaveNumbers}}.

to_numbers(NumbersBin) ->
    [binary_to_integer(Bin)
     || Bin <- binary:split(NumbersBin, <<" ">>, ['global', 'trim']),
        Bin =/= <<>>
    ].

part2(Cards) ->
    Count = count_scratchcards(Cards),
    io:format("total scratchcards: ~p~n", [Count]).

count_scratchcards(Cards) ->
    MapCardTo = maps:fold(fun count_scratchcard/3, #{}, Cards),
    {_, Count} = maps:fold(fun count_total/3, {MapCardTo, 0}, MapCardTo),
    Count.

count_scratchcard(CardNo, Numbers, MapCardTo) ->
    MapCardTo#{CardNo => matching_numbers(Numbers)}.

count_total(_CardNo, 0, {MapCardTo, Count}) ->
    {MapCardTo, Count+1};
count_total(CardNo, N, {MapCardTo, Count}) ->
    Copies = [CardNo+X || X <- lists:seq(1, N)],
    lists:foldl(fun count_copy/2, {MapCardTo, Count+1}, Copies).

count_copy(CopyCardNo, {MapCardTo, Count}) ->
    count_total(CopyCardNo, maps:get(CopyCardNo, MapCardTo, 0), {MapCardTo, Count}).

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.
