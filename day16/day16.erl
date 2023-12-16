-module(day16).

-export([energized_tiles_count/2
        ,print_grid/1
        ]).

energized_tiles_count(Input, Part) ->
    {H, W, Grid} = parse_grid(Input),
    case Part of
        1 -> energized_tiles_count(H, W, Grid, [{0, 0, 'right'}]);
        2 ->
            RightOptions = [{I, 0, 'right'} || I <- lists:seq(0, H - 1)],
            DownOptions = [{0, J, 'down'} || J <- lists:seq(0, W - 1)],
            LeftOptions = [{I, W - 1, 'left'} || I <- lists:seq(0, H - 1)],
            UpOptions = [{H - 1, J, 'up'} || J <- lists:seq(0, W - 1)],
            StartOptions = [{0, 0, 'right'}, {0, 0, 'down'},
                            {0, W - 1, 'left'}, {0, W - 1, 'down'},
                            {H - 1, 0, 'right'}, {H - 1, 0, 'up'},
                            {H - 1, W - 1, 'left'}, {H - 1, W - 1, 'up'}
                           ] ++ RightOptions ++ DownOptions ++ LeftOptions ++ UpOptions,
            lists:max([energized_tiles_count(H, W, Grid, [StartState])
                       || StartState <- StartOptions
                      ])
    end.

energized_tiles_count(H, W, Grid, StartStates) ->
    EnergizedTiles = follow_beam(StartStates, {H, W, Grid}, sets:new()),
    length(EnergizedTiles).

follow_beam([], {_H, _W, _Grid}, Visited) ->
    lists:uniq(fun({I, J, _Dir}) -> {I, J} end, sets:to_list(Visited));
follow_beam([{I, J, _Dir} | States], {H, W, Grid}, Visited)
  when I < 0; I >= H; J < 0; J >= W ->
    follow_beam(States, {H, W, Grid}, Visited);
follow_beam([State | States], {H, W, Grid}, Visited) ->
    case sets:is_element(State, Visited) of
        'true' -> follow_beam(States, {H, W, Grid}, Visited);
        'false' ->
            Visited1 = sets:add_element(State, Visited),
            NextStates = next_states(State, {H, W, Grid}),
            follow_beam(States ++ NextStates, {H, W, Grid}, Visited1)
    end.

next_states({I, J, Dir}, {_H, _W, Grid}) ->
    Row = array:get(I, Grid),
    Cell = array:get(J, Row),
    case {Dir, Cell} of
        {'right', $.} -> [{I, J + 1, Dir}];
        {'right', $|} -> [{I - 1, J, 'up'}, {I + 1, J, 'down'}];
        {'right', $-} -> [{I, J + 1, 'right'}];
        {'right', $\\} -> [{I + 1, J, 'down'}];
        {'right', $/} -> [{I - 1, J, 'up'}];
        {'down', $.} -> [{I + 1, J, Dir}];
        {'down', $|} -> [{I + 1, J, 'down'}];
        {'down', $-} -> [{I, J - 1, 'left'}, {I, J + 1, 'right'}];
        {'down', $\\} -> [{I, J + 1, 'right'}];
        {'down', $/} -> [{I, J - 1, 'left'}];
        {'left', $.} -> [{I, J - 1, Dir}];
        {'left', $|} -> [{I - 1, J, 'up'}, {I + 1, J, 'down'}];
        {'left', $-} -> [{I, J - 1, 'left'}];
        {'left', $\\} -> [{I - 1, J, 'up'}];
        {'left', $/} -> [{I + 1, J, 'down'}];
        {'up', $.} -> [{I - 1, J, Dir}];
        {'up', $|} -> [{I - 1, J, 'up'}];
        {'up', $-} -> [{I, J - 1, 'left'}, {I, J + 1, 'right'}];
        {'up', $\\} -> [{I, J - 1, 'left'}];
        {'up', $/} -> [{I, J + 1, 'right'}]
    end.

parse_grid(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    H = length(Lines),
    parse_grid(Lines, 0, {H, 0, array:new(H)}).

parse_grid([], _I, Acc) -> Acc;
parse_grid([Line | Lines], I, {H, _W, Grid}) ->
    W = size(Line),
    Row = parse_row(Line, 0, array:new(W)),
    Grid1 = array:set(I, Row, Grid),
    parse_grid(Lines, I + 1, {H, W, Grid1}).

parse_row(<<>>, _J, Row) -> Row;
parse_row(<<C, Rest/binary>>, J, Row) ->
    Row1 = array:set(J, C, Row),
    parse_row(Rest, J + 1, Row1).

print_grid(Grid) ->
    _ = [print_row(Row) || Row <- array:to_list(Grid)],
    'ok'.

print_row(Row) ->
    RowBin = array:foldl(fun(_I, C, Acc) -> <<Acc/binary, C>> end, <<>>, Row),
    io:format("~s~n", [RowBin]).
