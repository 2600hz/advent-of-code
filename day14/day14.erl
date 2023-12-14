-module(day14).

-export([total_load/2]).

-define(ONE_BILLION, 1_000_000_000).

total_load(Input, Part) ->
    Grid = parse_grid(Input),
    case Part of
        1 ->
            Grid1 = tilt(Grid, 'up'),
            grid_load(Grid1);
        2 ->
            {Start, Period, Memory} = find_cycle(Grid, ?ONE_BILLION),
            OrderedMemory = [V || {_I, V} <- lists:sort(maps:values(Memory))],
            lists:nth(1 + Start + (?ONE_BILLION - Start - 1) rem Period
                     ,OrderedMemory
                     )
    end.

find_cycle(Grid, N) -> find_cycle(Grid, N, maps:new()).

find_cycle(_Grid, 0, _Memory) -> 'undefined';
find_cycle(Grid, N, Memory) ->
    Grid1 = cycle(Grid),
    GridHash = erlang:phash2(Grid1),
    case maps:get(GridHash, Memory, 'undefined') of
        'undefined' ->
            GridLoad = grid_load(Grid1),
            find_cycle(Grid1, N - 1, Memory#{GridHash => {?ONE_BILLION - N, GridLoad}});
        {Start, _GridLoad} ->
            {Start, ?ONE_BILLION - N - Start, Memory}
    end.

grid_load(Grid) -> grid_load(Grid, 0, 0).

grid_load({_H, _W, []}, _Row, Load) -> Load;
grid_load({H, W, [{I, _J, 'round'} | Grid]}, I, Load) ->
    grid_load({H, W, Grid}, I, Load + H - I);
grid_load({H, W, [{I, _J, 'cube'} | Grid]}, I, Load) ->
    grid_load({H, W, Grid}, I, Load);
grid_load({H, W, [{I, _J, _Code} | _]=Grid}, _Row, Load) ->
    grid_load({H, W, Grid}, I, Load).

cycle(Grid) ->
    Grid1 = tilt(Grid, 'up'),
    Grid2 = tilt(Grid1, 'left'),
    Grid3 = tilt(Grid2, 'down'),
    Grid4 = tilt(Grid3, 'right'),
    Grid4.

tilt(Grid, 'left') -> tilt(Grid, 0);
tilt(Grid, 'up') -> tilt(Grid, 1);
tilt(Grid, 'right') -> tilt(Grid, 2);
tilt(Grid, 'down') -> tilt(Grid, 3);
tilt(Grid, N) ->
    Grid1 = rotate(Grid, N),
    Grid2 = tilt(Grid1, 0, 0, []),
    rotate(Grid2, -N).

tilt({H, W, []}, _Row, _LJ, Tilted) -> {H, W, lists:reverse(Tilted)};
tilt({H, W, [{I, _J, 'round'} | Grid]}, I, LJ, Tilted) ->
    tilt({H, W, Grid}, I, LJ + 1, [{I, LJ, 'round'} | Tilted]);
tilt({H, W, [{I, J, 'cube'} | Grid]}, I, _LJ, Tilted) ->
    tilt({H, W, Grid}, I, J + 1, [{I, J, 'cube'} | Tilted]);
tilt({H, W, [{I, _J, _Code} | _]=Grid}, _Row, _LJ, Tilted) ->
    tilt({H, W, Grid}, I, 0, Tilted).

rotate(Grid, N) when N < 0 -> rotate(Grid, N + 4);
rotate(Grid, N) -> rotate(Grid, N, []).

rotate({H, W, Rotated}, 0, _Acc) -> {H, W, lists:sort(Rotated)};
rotate({H, W, []}, N, Rotated) ->
    rotate({W, H, Rotated}, N - 1, []);
rotate({H, H, [{I, J, Code} | Grid]}, N, Rotated) ->
    rotate({H, H, Grid}, N, [{H - J - 1, I, Code} | Rotated]).

parse_grid(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    parse_grid(Lines, 0, 0, {0, 0, []}).

parse_grid([], I, _J, {_H, W, Grid}) -> {I, W, lists:reverse(Grid)};
parse_grid([<<>> | Lines], I, J, {H, _W, Grid}) ->
    parse_grid(Lines, I + 1, 0, {H, J, Grid});
parse_grid([<<C:8, Rest/binary>> | Lines], I, J, {H, W, Grid})
  when C =:= $O; C =:= $# ->
    parse_grid([Rest | Lines], I, J + 1, {H, W, [{I, J, code(C)} | Grid]});
parse_grid([<<_:8, Rest/binary>> | Lines], I, J, {H, W, Grid}) ->
    parse_grid([Rest | Lines], I, J + 1, {H, W, Grid}).

code($O) -> 'round';
code($#) -> 'cube'.
