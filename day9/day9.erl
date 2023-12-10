-module(day9).

-export([sum_extrapolated_values/2]).

sum_extrapolated_values(Input, Part) ->
    Histories = parse_input(Input),
    Histories1 = case Part of
                     1 -> Histories;
                     2 -> [lists:reverse(History) || History <- Histories]
                 end,
    Differences = [generate_differences(History) || History <- Histories1],
    Finals = [finals(Diffs) || Diffs <- Differences],
    NextValues = [lists:sum(Final) || Final <- Finals],
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

finals(Differences) -> [lists:last(Diff) || Diff <- Differences].

parse_input(Input) ->
    Histories = binary:split(Input, <<"\n">>, ['global']),
    [parse_history(History) || History <- Histories].

parse_history(History) ->
    Values = binary:split(History, <<" ">>, ['global']),
    [binary_to_integer(Value) || Value <- Values].
