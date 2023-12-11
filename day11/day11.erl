-module(day11).

-export([sum_distances_between_galaxies/2]).

sum_distances_between_galaxies(Input, Part) ->
    Cells = parse_grid(Input),
    {EmptyRows, EmptyCols} = empty_rows_cols(Cells),
    ScalingFactor = case Part of
                        1 -> 1;
                        2 -> 1_000_000 - 1
                    end,
    Pairs = pairs(Cells),
    Distances = [distance(A, B, EmptyRows, EmptyCols, ScalingFactor)
                 || {A, B} <- Pairs
                ],
    lists:sum(Distances).

pairs(Cells) -> pairs(Cells, Cells, []).

pairs([], _Cs2, Acc) -> Acc;
pairs([_C | Cs], [], Acc) -> pairs(Cs, Cs, Acc);
pairs([C | Cs], [C | Cs2], Acc) -> pairs([C | Cs], Cs2, Acc);
pairs([C1 | Cs1], [C2 | Cs2], Acc) -> pairs([C1 | Cs1], Cs2, [{C1, C2} | Acc]).

distance({I1, J1}, {I2, J2}, EmptyRows, EmptyCols, ScalingFactor) ->
    MinI = min(I1, I2),
    MaxI = max(I1, I2),
    MinJ = min(J1, J2),
    MaxJ = max(J1, J2),
    EmptyRowsN = length([I || I <- EmptyRows, I < MaxI, I > MinI]) * ScalingFactor,
    EmptyColsN = length([J || J <- EmptyCols, J < MaxJ, J > MinJ]) * ScalingFactor,
    distance({I1, J1}, {I2, J2}) + EmptyRowsN + EmptyColsN.

distance({I1, J1}, {I2, J2}) -> abs(I1 - I2) + abs(J1 - J2).

empty_rows_cols(Cells) -> {empty_rows(Cells), empty_cols(Cells)}.

empty_rows(Cells) -> empty_rows([I || {I, _} <- Cells], {0, []}).

empty_cols(Cells) -> empty_rows([J || {_, J} <- Cells], {0, []}).

empty_rows(Is, {N, Acc}) when N >= length(Is) -> Acc;
empty_rows(Is, {N, Acc}) ->
    Acc1 = case lists:member(N, Is) of
               'true' -> Acc;
               'false' -> [N | Acc]
           end,
    empty_rows(Is, {N + 1, Acc1}).

parse_grid(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    parse_rows(Lines).

parse_rows(Lines) -> parse_rows(Lines, {0, []}).

parse_rows([], {_I, Acc}) -> Acc;
parse_rows([Line | Lines], {I, Acc}) ->
    Acc1 = parse_row(Line, {I, 0, Acc}),
    parse_rows(Lines, {I + 1, Acc1}).

parse_row(<<>>, {_I, _J, Acc}) -> Acc;
parse_row(<<$., Line/binary>>, {I, J, Acc}) ->
    parse_row(Line, {I, J + 1, Acc});
parse_row(<<$#, Line/binary>>, {I, J, Acc}) ->
    parse_row(Line, {I, J + 1, [{I, J} | Acc]}).
