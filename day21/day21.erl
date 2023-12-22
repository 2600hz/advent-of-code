-module(day21).

-export([reachable_plots/2]).

-define(GOAL_PART1, 64).
-define(GOAL_PART2, 26501365).

reachable_plots(Input, Part) ->
    {Start, H, W, Grid} = parse_grid(Input),
    EndCond = case Part of
                  1 -> fun(N, _Grid, _Visited, Context) -> {N =:= ?GOAL_PART1, Context} end;
                  2 -> fun end_cond_fun/4
              end,
    NeighboursFun = neighbours_fun(Part),
    Options = [{'context', []}
              ,{'end_cond_fun', EndCond}
              ,{'neighbours_fun', NeighboursFun}
              ],
    {Plots, Context} = step([Start], {H, W, Grid}, Options),
    case Part of
        1 -> sets:size(Plots);
        2 ->
             %% We care about the number of recurrences of the full cycle, not
             %% the total number of steps taken
            X = ?GOAL_PART2 div H,
            math2:newton_interpolation_polynomial(X, lists:reverse(Context))
    end.

end_cond_fun(N, {H, _W, _Grid}, Visited, Context) when N rem H =:= ?GOAL_PART2 rem H ->
    io:format("Step ~b: ~b~n", [N, sets:size(Visited)]),
    Context1 = [sets:size(Visited) | Context],
    IsEnd = length(Context1) =:= 3,
    {IsEnd, Context1};
end_cond_fun(_, _, _, Context) ->
    {'false', Context}.

neighbours_fun(Part) ->
    fun(Coord, Grid) -> neighbours(Coord, Grid, Part) end.

neighbours({I, J}, {H, W, Grid}, Part) ->
    Neighbours = [{I - 1, J}, {I + 1, J}, {I, J - 1}, {I, J + 1}],
    [Neighbour || Neighbour <- Neighbours,
                  is_valid_neighbour(Neighbour, {H, W, Grid}, Part)
    ].

is_valid_neighbour({I, J}, {H, W, Grid}, Part) when I < 0 ->
    case Part of
        1 -> 'false';
        2 -> is_valid_neighbour({I + H, J}, {H, W, Grid}, Part)
    end;
is_valid_neighbour({I, J}, {H, W, Grid}, Part) when J < 0 ->
    case Part of
        1 -> 'false';
        2 -> is_valid_neighbour({I, J + W}, {H, W, Grid}, Part)
    end;
is_valid_neighbour({I, J}, {H, W, _Grid}, 1) when I >= H; J >= W ->
    'false';
is_valid_neighbour({I, J}, {H, W, Grid}, _) ->
    Row = array:get(I rem H, Grid),
    array:get(J rem W, Row) =:= $..

step(Plots, Grid, Options) ->
    Context = proplists:get_value('context', Options),
    step(0, Plots, Grid, Options, Context, sets:new()).

step(N, [], Grid, Options, Context, Acc) ->
    EndCondFun = proplists:get_value('end_cond_fun', Options),
    case EndCondFun(N + 1, Grid, Acc, Context) of
        {'true', Context1} -> {Acc, Context1};
        {'false', Context1} ->
            step(N + 1, sets:to_list(Acc), Grid, Options, Context1, sets:new())
    end;
step(N, [Plot | Plots], Grid, Options, Context, Acc) ->
    NeighboursFun = proplists:get_value('neighbours_fun', Options),
    Neighbours = NeighboursFun(Plot, Grid),
    Acc1 = lists:foldl(fun sets:add_element/2, Acc, Neighbours),
    step(N, Plots, Grid, Options, Context, Acc1).

parse_grid(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    {Start, W, Grid} = parse_grid(Lines, 0, {'undefined', 'undefined', array:new()}),
    {Start, length(Lines), W, Grid}.

parse_grid([], _I, Acc) -> Acc;
parse_grid([Line | Lines], I, {Start, _W, Grid}) ->
    {NewStartJ, W, Row} = parse_row(Line),
    Start1 = case NewStartJ of
                 'undefined' -> Start;
                 _ -> {I, NewStartJ}
             end,
    Grid1 = array:set(I, Row, Grid),
    parse_grid(Lines, I + 1, {Start1, W, Grid1}).

parse_row(Line) ->
    {Start, Row} = parse_row(Line, 0, {'undefined', array:new()}),
    {Start, byte_size(Line), Row}.

parse_row(<<>>, _J, Acc) -> Acc;
parse_row(<<$S, Rest/binary>>, J, {_Start, Row}) ->
    Row1 = array:set(J, $., Row),
    parse_row(Rest, J + 1, {J, Row1});
parse_row(<<C:8, Rest/binary>>, J, {Start, Row}) ->
    Row1 = array:set(J, C, Row),
    parse_row(Rest, J + 1, {Start, Row1}).
