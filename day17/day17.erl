-module(day17).

-export([minimum_heat_loss/2]).

minimum_heat_loss(Input, Part) ->
    {H, W, Grid} = parse_grid(Input),
    Goal = {H - 1, W - 1},
    StartContext = 'undefined',
    Start = {0, 0, StartContext},
    NeighboursFun = fun(Node, Grid1) -> neighbours(Node, Grid1, Part) end,
    Options = [{'heuristic_fun', fun heuristic/2}
              ,{'neighbours_fun', NeighboursFun}
              ,{'cost_fun', fun cost/3}
              ],
    a_star(Start, Goal, {H, W, Grid}, Options).

heuristic({I, J, _Context}, Goal) -> manhattan_distance({I, J}, Goal).

neighbours({I, J, 'undefined'}, Grid, Part) ->
    neighbours({I, J, 'vertical'}, Grid, Part)
        ++ neighbours({I, J, 'horizontal'}, Grid, Part);
neighbours({I, J, 'vertical'}, {_H, W, _Grid}, Part) ->
    [{I, J + D, 'horizontal'}
     || D <- neighbours_seq(J, 0, W - 1, Part)
    ];
neighbours({I, J, 'horizontal'}, {H, _W, _Grid}, Part) ->
    [{I + D, J, 'vertical'}
     || D <- neighbours_seq(I, 0, H - 1, Part)
    ].

neighbours_seq(N, Min, Max, 1) ->
    [I || I <- lists:seq(-3, 3), I =/= 0, N + I >= Min, N + I =< Max];
neighbours_seq(N, Min, Max, 2) ->
    [I || I <- lists:seq(-10, 10), I =< -4 orelse I >= 4, N + I >= Min, N + I =< Max].

cost({I1, J1, _Context1}, {I2, J2, _Context2}, {_H, _W, Grid}) ->
    lists:sum(cost_seq({I1, J1}, {I2, J2}, Grid)).

cost_seq({I, J1}, {I, J2}, Grid) when J1 < J2 ->
    [array:get(J, array:get(I, Grid)) || J <- lists:seq(J1 + 1, J2)];
cost_seq({I, J1}, {I, J2}, Grid) when J1 > J2 ->
    [array:get(J, array:get(I, Grid)) || J <- lists:seq(J2, J1 - 1)];
cost_seq({I1, J}, {I2, J}, Grid) when I1 < I2 ->
    [array:get(J, array:get(I, Grid)) || I <- lists:seq(I1 + 1, I2)];
cost_seq({I1, J}, {I2, J}, Grid) when I1 > I2 ->
    [array:get(J, array:get(I, Grid)) || I <- lists:seq(I2, I1 - 1)].

a_star(Start, Goal, Grid, Options) ->
    HeuristicFun = proplists:get_value('heuristic_fun', Options),
    GScore = 0,
    FScore = GScore + HeuristicFun(Start, Goal),
    CameFrom = 'undefined',
    State = #{Start => {GScore, CameFrom}},
    %% TODO: oh dear lord help me with this
    %% Implement a priority queue or import a library
    a_star([{FScore, Start}], Goal, Grid, Options, State).

a_star([], _Goal, _Grid, _Options, _State) -> {'error', 'not_found'};
a_star([{_FScore, {I, J, _Context}=Cur} | _Open], {I, J}, _Grid, _Options, State) ->
    #{Cur := {GScore, _CameFrom}} = State,
    {'ok', GScore, reconstruct_path(State, Cur)};
a_star([{_FScore, Cur} | Open], Goal, Grid, Options, State) ->
    NeighboursFun = proplists:get_value('neighbours_fun', Options),
    {Open1, State1} = update_state(NeighboursFun(Cur, Grid)
                                  ,Cur
                                  ,Open
                                  ,Goal
                                  ,Grid
                                  ,Options
                                  ,State
                                  ),
    a_star(Open1, Goal, Grid, Options, State1).

update_state([], _Cur, Open, _Goal, _Grid, _Options, State) -> {Open, State};
update_state([Neighbour | Neighbours], Cur, Open, Goal, Grid, Options, State) ->
    HeuristicFun = proplists:get_value('heuristic_fun', Options),
    CostFun = proplists:get_value('cost_fun', Options),
    #{Cur := {GScore1, _CameFrom1}} = State,
    TentativeGScore = GScore1 + CostFun(Cur, Neighbour, Grid),

    IsBetterPath = case maps:get(Neighbour, State, 'undefined') of
                       'undefined' -> 'true';
                       {GScore2, _CameFrom2} -> TentativeGScore < GScore2
                   end,

    case IsBetterPath of
        'true' ->
            NewFScore = TentativeGScore + HeuristicFun(Neighbour, Goal),
            State1 = State#{Neighbour => {TentativeGScore, Cur}},
            Open1 = lists:sort([{NewFScore, Neighbour} | Open]);
        'false' ->
            State1 = State,
            Open1 = Open
    end,
    update_state(Neighbours, Cur, Open1, Goal, Grid, Options, State1).

reconstruct_path(State, Node) -> reconstruct_path(State, Node, []).

reconstruct_path(State, Node, Path) ->
    #{Node := {_GScore, CameFrom}} = State,
    Path1 = [Node | Path],
    case CameFrom of
        'undefined' -> Path1;
        _ -> reconstruct_path(State, CameFrom, Path1)
    end.

manhattan_distance({I1, J1}, {I2, J2}) -> abs(I2 - I1) + abs(J1 - J2).

parse_grid(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    {W, Grid} = parse_grid(Lines, 0, {'undefined', array:new()}),
    {length(Lines), W, Grid}.

parse_grid([], _I, Acc) -> Acc;
parse_grid([Line | Lines], I, {_W, Grid}) ->
    {W, Row} = parse_row(Line),
    Grid1 = array:set(I, Row, Grid),
    parse_grid(Lines, I + 1, {W, Grid1}).

parse_row(Line) ->
    {byte_size(Line), parse_row(Line, 0, array:new())}.

parse_row(<<>>, _J, Row) -> Row;
parse_row(<<C:8, Rest/binary>>, J, Row) ->
    Row1 = array:set(J, C - $0, Row),
    parse_row(Rest, J + 1, Row1).
