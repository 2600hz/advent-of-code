-module(day10).

-export([steps_farthest_from_start/1
        ,enclosed_tiles_count/1
        ,print_grid/1
        ]).

steps_farthest_from_start(Input) ->
    {Steps, _Visited} = parse_and_traverse_grid(Input),
    Steps div 2.

enclosed_tiles_count(Input) ->
    {_Steps, Visited} = parse_and_traverse_grid(Input),
    Coords = [Coords || {Coords, _Info} <- lists:keysort(2, maps:to_list(Visited))],
    Area = polygon_area(Coords),
    BoundaryPointsCount = length(Coords),
    interior_points_count(floor(Area), BoundaryPointsCount).

parse_and_traverse_grid(Input) ->
    {Start, Grid, Height, Width} = parse_grid(Input),
    traverse_grid(Start, Grid, Height, Width).

traverse_grid(Start, Grid, Height, Width) ->
    traverse_grid(Start, Grid, Height, Width, 0, maps:new()).

traverse_grid({I, J}, _Grid, _H, _W, Steps, Visited)
  when is_map_key({I, J}, Visited) -> {Steps, Visited};
traverse_grid({I, J}, Grid, H, W, Steps, Visited) ->
    Open = open_cells({I, J, H, W}, Grid),
    Steps1 = Steps + 1,
    Row = array:get(I, Grid),
    Cell = array:get(J, Row),
    Visited1 = Visited#{{I, J} => {Steps1, Cell}},
    Paths = [traverse_grid(OCell, Grid, H, W, Steps1, Visited1)
                || OCell <- Open
            ],
    longest_path(Paths).

longest_path(Paths) -> longest_path(Paths, {0, 'undefined'}).

longest_path([], LPath) -> LPath;
longest_path([{Steps, Visited} | Paths], {LSteps, _LVisited})
  when Steps > LSteps ->
    longest_path(Paths, {Steps, Visited});
longest_path([_Path | Paths], {LSteps, LVisited}) ->
    longest_path(Paths, {LSteps, LVisited}).

open_cells({I, J, H, W}, Grid) ->
    open_cells([{I - 1, J}, {I + 1, J}, {I, J - 1}, {I, J + 1}]
              ,{I, J, H, W}
              ,Grid
              ,[]
              ).

open_cells([], _Bounds, _Grid, Acc) -> Acc;
open_cells([{I, J} | Cells], {I0, J0, H, W}, Grid, Acc)
  when I < 0; J < 0; I >= H; J >= W ->
    open_cells(Cells, {I0, J0, H, W}, Grid, Acc);
open_cells([{I, J} | Cells], {I0, J0, H, W}, Grid, Acc) ->
    Acc1 = case is_open_cell({I, J}, {I0, J0}, Grid) of
               'true' -> [{I, J} | Acc];
               'false' -> Acc
           end,
    open_cells(Cells, {I0, J0, H, W}, Grid, Acc1).

is_open_cell({I, J}, {I0, J0}, Grid) ->
    Row = array:get(I, Grid),
    Cell = array:get(J, Row),
    case Cell of
        'ns' when J =:= J0 -> 'true';
        'ew' when I =:= I0 -> 'true';
        'ne' when J =:= J0 - 1; I =:= I0 + 1 -> 'true';
        'nw' when J =:= J0 + 1; I =:= I0 + 1 -> 'true';
        'sw' when J =:= J0 + 1; I =:= I0 - 1 -> 'true';
        'se' when J =:= J0 - 1; I =:= I0 - 1 -> 'true';
        _ -> 'false'
    end.

polygon_area(Coords) ->
    polygon_area(Coords, hd(Coords), 0).

polygon_area([{Y1, X1}], {Y2, X2}, Area) ->
    TriangleArea = triangle_area({Y1, X1}, {Y2, X2}),
    abs(Area + TriangleArea);
polygon_area([{Y1, X1}, {Y2, X2} | Coords], Start, Area) ->
    TriangleArea = triangle_area({Y1, X1}, {Y2, X2}),
    polygon_area([{Y2, X2} | Coords], Start, Area + TriangleArea).

triangle_area({Y1, X1}, {Y2, X2}) -> (X1 * Y2 - X2 * Y1) / 2.

interior_points_count(Area, BoundaryPointsCount) ->
    Area - BoundaryPointsCount div 2 + 1.

parse_grid(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    parse_rows(Lines).

parse_rows(Lines) ->
    {Start, Rows, Width} = parse_rows(Lines, {'undefined', 0, [], 0}),
    {Start, Rows, array:size(Rows), Width}.

parse_rows([], {Start, _I, Rows, Width}) ->
    {Start, array:from_list(lists:reverse(Rows)), Width};
parse_rows([Line | Rest], {Start, I, Rows, _Width}) ->
    {RowStart, Row, Width} = parse_row(Line),
    Start1 = case RowStart of
                 'undefined' -> Start;
                 _ -> {I, RowStart}
             end,
    parse_rows(Rest, {Start1, I + 1, [Row | Rows], Width}).

parse_row(Line) ->
    {Start, Row} = parse_row(Line, {'undefined', 0, []}),
    {Start, Row, byte_size(Line)}.

parse_row(<<>>, {Start, _J, Row}) ->
    {Start, array:from_list(lists:reverse(Row))};
parse_row(<<Char, Rest/binary>>, {Start, J, Row}) ->
    Parsed = parse_char(Char),
    Start1 = case Parsed of
                 'start' -> J;
                 _ -> Start
             end,
    parse_row(Rest, {Start1, J + 1, [Parsed | Row]}).

parse_char($|) -> 'ns';
parse_char($-) -> 'ew';
parse_char($L) -> 'ne';
parse_char($J) -> 'nw';
parse_char($7) -> 'sw';
parse_char($F) -> 'se';
parse_char($.) -> 'ground';
parse_char($S) -> 'start'.

print_grid(Grid) ->
    Repr = repr_grid(Grid),
    _ = [io:format("~ts~n", [Row]) || Row <- Repr],
    'ok'.

repr_grid(Grid) ->
    [repr_row(Row) || Row <- array:to_list(Grid)].

repr_row(Row) ->
    [repr_cell(Cell) || Cell <- array:to_list(Row)].

repr_cell('ns') -> $|;
repr_cell('ew') -> $-;
repr_cell('ne') -> <<"└"/utf8>>;
repr_cell('nw') -> <<"┘"/utf8>>;
repr_cell('sw') -> <<"┐"/utf8>>;
repr_cell('se') -> <<"┌"/utf8>>;
repr_cell('ground') -> $ ;
repr_cell('start') -> $S.
