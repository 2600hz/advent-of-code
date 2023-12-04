-module(day4).

-export([total_points/1
        ,total_scratchcards/1
        ]).

total_points(Input) ->
    Cards = parse_cards(Input),
    CardsWithPoints = [add_points(Card) || Card <- Cards],
    lists:sum([Points || {_, _, _, Points} <- CardsWithPoints]).

total_scratchcards(Input) ->
    Cards = parse_cards(Input),
    CardsWithWinningNumberCount = [add_winning_number_count(Card) || Card <- Cards],
    CardsWithCopies = add_copies(CardsWithWinningNumberCount),
    lists:sum([Copies || {_, _, _, _, Copies} <- CardsWithCopies]).

add_copies(Cards) -> add_copies(Cards, []).

add_copies([], Acc) -> Acc;
add_copies([{_CardNumber, _, _, WinningNumberCount, Copies}=Card | Cards], Acc) ->
    if WinningNumberCount =:= 0 ->
            add_copies(Cards, [Card | Acc]);
       WinningNumberCount > 0 ->
            {IncCards, Rest} = lists:split(WinningNumberCount, Cards),
            Increased = increase_copies(IncCards, Copies),
            add_copies(Increased ++ Rest, [Card | Acc])
    end.

increase_copies(Cards, N) ->
    [{CardNumber, WinningNumbers, Numbers, WinningNumberCount, Copies + N}
     || {CardNumber, WinningNumbers, Numbers, WinningNumberCount, Copies} <- Cards
    ].

add_points({CardNumber, WinningNumbers, Numbers}) ->
    Points = case winning_number_count(WinningNumbers, Numbers) of
                 0 -> 0;
                 N -> trunc(math:pow(2, N - 1))
             end,
    {CardNumber, WinningNumbers, Numbers, Points}.

add_winning_number_count({CardNumber, WinningNumbers, Numbers}) ->
    WinningNumberCount = winning_number_count(WinningNumbers, Numbers),
    {CardNumber, WinningNumbers, Numbers, WinningNumberCount, 1}.

winning_number_count(WinningNumbers, Numbers) ->
    WinningNumbersSet = sets:from_list(WinningNumbers),
    NumbersSet = sets:from_list(Numbers),
    WonNumbers = sets:intersection(WinningNumbersSet, NumbersSet),
    sets:size(WonNumbers).

parse_cards(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    [parse_card(Line) || Line <- Lines].

parse_card(Input) ->
    [<<"Card ", CardNumberBin/binary>>, WinningNumbersInput, NumbersInput] =
      binary:split(Input, [<<":">>, <<"|">>], ['global', 'trim_all']),
    WinningNumbers = binary:split(WinningNumbersInput, <<" ">>, ['global', 'trim_all']),
    Numbers = binary:split(NumbersInput, <<" ">>, ['global', 'trim_all']),
    {parse_card_number(CardNumberBin), WinningNumbers, Numbers}.

parse_card_number(CardNumber) -> binary_to_integer(string:trim(CardNumber)).
