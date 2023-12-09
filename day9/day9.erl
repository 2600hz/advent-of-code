-module(day9).

-export([sum_extrapolated_values/2]).

sum_extrapolated_values(Input, Part) ->
    Histories = parse_input(Input),
    Differences = [generate_differences(History) || History <- Histories],
    Finals = [finals(Diffs, Part) || Diffs <- Differences],
    NextValues = case Part of
                     1 -> [lists:sum(Final) || Final <- Finals];
                     2 -> [progressively_sub(Heads) || Heads <- Finals]
                 end,
    lists:sum(NextValues).

generate_differences(History) -> generate_differences(History, [[], History]).

generate_differences([_], [Acc | Differences]) ->
    Acc1 = lists:reverse(Acc),
    case lists:all(fun(X) -> X =:= 0 end, Acc1) of
        'true' -> lists:reverse([Acc1 | Differences]);
        'false' -> generate_differences(Acc1, [[], Acc1 | Differences])
    end;
generate_differences([A, B | Rest], [Acc | Differences]) ->
    generate_differences([B | Rest], [[B - A | Acc] | Differences]).

finals(Differences, 1) -> [lists:last(Diff) || Diff <- Differences];
finals(Differences, 2) -> [hd(Diff) || Diff <- Differences].

progressively_sub(Heads) ->
    [0 | Heads1] = lists:reverse(Heads),
    lists:foldl(fun 'erlang':'-'/2, 0, Heads1).

parse_input(Input) ->
    Histories = binary:split(Input, <<"\n">>, ['global']),
    [parse_history(History) || History <- Histories].

parse_history(History) ->
    Values = binary:split(History, <<" ">>, ['global']),
    [binary_to_integer(Value) || Value <- Values].
