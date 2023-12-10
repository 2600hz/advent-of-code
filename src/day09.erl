-module(day09).

-export([run/0]).

%% https://adventofcode.com/2023/day/9/
%% sum of extrapolation: 1904165718
%% sum of first extrapolations: 964

run() ->
    Histories = parse_histories(input("day09.txt")),
    Predictions = predict_next(Histories),
    part1(Predictions),
    part2(Predictions).

part1(Predictions) ->
    Lasts = [L || {_, L} <- Predictions],
    io:format("sum of extrapolation: ~p~n", [lists:sum(Lasts)]).

part2(Predictions) ->
    Firsts = [firsts_to_prediction(F) || {F, _} <- Predictions],
    io:format("sum of first extrapolations: ~w~n", [lists:sum(Firsts)]).

firsts_to_prediction(Firsts) ->
    lists:foldl(fun(First, Sum) -> First - Sum end, 0, Firsts).

predict_next(Histories) ->
    predict_next(Histories, []).

predict_next([], Predictions) ->
    lists:reverse(Predictions);
predict_next([History | Histories], Predictions) ->
    Prediction = predict_next_h(History),
    predict_next(Histories, [Prediction | Predictions]).

predict_next_h(History) ->
    predict_next_h(History, {[], 0}).

predict_next_h(History, {Fs, L}) ->
    {First, Last, Diffs} = diffs(History),
    case lists:all(fun is_zero/1, Diffs) of
        'true' -> {[First | Fs], L+Last};
        'false' ->
            predict_next_h(Diffs, {[First | Fs], L+Last})
    end.

diffs([First | _]=History) ->
    diffs(History, {First, []}).

diffs([A, B], {First, Diffs}) ->
    {First, B, lists:reverse([B-A | Diffs])};
diffs([A, B | Rest], {First, Diffs}) ->
    diffs([B | Rest], {First, [B-A | Diffs]}).

is_zero(N) -> N =:= 0.

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
