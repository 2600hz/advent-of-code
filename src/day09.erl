-module(day09).

-export([run/0]).

%% https://adventofcode.com/2023/day/9/
%% sum of extrapolation: 1904165718

run() ->
    Histories = parse_histories(input("day09.txt")),
    part1(Histories),
    part2(Histories).

part1(Histories) ->
    Predictions = predict_next(Histories),
    io:format("sum of extrapolation: ~p~n", [lists:sum(Predictions)]).

predict_next(Histories) ->
    predict_next(Histories, []).

predict_next([], Predictions) ->
    lists:reverse(Predictions);
predict_next([History | Histories], Predictions) ->
    Prediction = predict_next_h(History),
    predict_next(Histories, [Prediction | Predictions]).

predict_next_h(History) ->
    predict_next_h(History, 0).

predict_next_h(History, Next) ->
    {Last, Diffs} = diffs(History),
    case lists:all(fun is_zero/1, Diffs) of
        'true' -> Next + Last;
        'false' ->
            predict_next_h(Diffs, Next + Last)
    end.

diffs(History) ->
    diffs(History, []).

diffs([A, B], Diffs) ->
    {B, lists:reverse([B-A | Diffs])};
diffs([A, B | Rest], Diffs) ->
    diffs([B | Rest], [B-A | Diffs]).

is_zero(N) -> N =:= 0.

part2(Input) ->
    Input.

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.

parse_histories(Input) ->
    Histories = binary:split(Input, <<$\n>>, ['global', 'trim']),
    [parse_history(H) || H <- Histories].

parse_history(History) ->
    [binary_to_integer(H) || H <- binary:split(History, <<" ">>, ['global', 'trim']),
                             H =/= <<>>
    ].
