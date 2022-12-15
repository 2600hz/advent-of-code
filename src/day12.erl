-module(day12).

%% --- Day 12: Hill Climbing Algorithm ---

%% You try contacting the Elves using your handheld device, but the
%% river you're following must be too low to get a decent signal.

%% You ask the device for a heightmap of the surrounding area (your
%% puzzle input). The heightmap shows the local area from above broken
%% into a grid; the elevation of each square of the grid is given by a
%% single lowercase letter, where a is the lowest elevation, b is the
%% next-lowest, and so on up to the highest elevation, z.

%% Also included on the heightmap are marks for your current position
%% (S) and the location that should get the best signal (E). Your
%% current position (S) has elevation a, and the location that should
%% get the best signal (E) has elevation z.

%% You'd like to reach E, but to save energy, you should do it in as
%% few steps as possible. During each step, you can move exactly one
%% square up, down, left, or right. To avoid needing to get out your
%% climbing gear, the elevation of the destination square can be at
%% most one higher than the elevation of your current square; that is,
%% if your current elevation is m, you could step to elevation n, but
%% not to elevation o. (This also means that the elevation of the
%% destination square can be much lower than the elevation of your
%% current square.)

%% For example:

%% Sabqponm
%% abcryxxl
%% accszExk
%% acctuvwj
%% abdefghi

%% Here, you start in the top-left corner; your goal is near the
%% middle. You could start by moving down or right, but eventually
%% you'll need to head toward the e at the bottom. From there, you can
%% spiral around to the goal:

%% v..v<<<<
%% >v.vv<<^
%% .>vv>E^^
%% ..v>>>^^
%% ..>>>>>^

%% In the above diagram, the symbols indicate whether the path exits
%% each square moving up (^), down (v), left (<), or right (>). The
%% location that should get the best signal is still E, and . marks
%% unvisited squares.

%% This path reaches the goal in 31 steps, the fewest possible.

%% What is the fewest steps required to move from your current
%% position to the location that should get the best signal?

%% --- Part Two ---

%% As you walk up the hill, you suspect that the Elves will want to
%% turn this into a hiking trail. The beginning isn't very scenic,
%% though; perhaps you can find a better starting point.

%% To maximize exercise while hiking, the trail should start as low as
%% possible: elevation a. The goal is still the square marked
%% E. However, the trail should still be direct, taking the fewest
%% steps to reach its goal. So, you'll need to find the shortest path
%% from any square at elevation a to the square marked E.

%% Again consider the example from above:

%% Sabqponm
%% abcryxxl
%% accszExk
%% acctuvwj
%% abdefghi

%% Now, there are six choices for starting position (five marked a,
%% plus the square marked S that counts as being at elevation a). If
%% you start at the bottom-left square, you can reach the goal most
%% quickly:

%% ...v<<<<
%% ...vv<<^
%% ...v>E^^
%% .>v>>>^^
%% >^>>>>>^

%% This path reaches the goal in only 29 steps, the fewest possible.

%% What is the fewest steps required to move starting from any square
%% with elevation a to the location that should get the best signal?

-export([run/0
        ,part1/0
        ,part2/0
        ]).

-record(graph, {start, goal, nodes}).

run() ->
    Input = input(),
    part1(Input),
    part2(Input).

part1() ->
    part1(input()).

part2() ->
    part2(input()).

part1(#graph{start=Start, goal=Goal, nodes=Nodes}) ->
    Steps = a_star(Nodes, Goal, Start),
    io:format("took ~p steps~n", [Steps]).

a_star(Nodes, Goal, Start) ->
    a_star(Nodes, Goal, [{Start, 0}], #{}).

a_star(_Nodes, Goal, [], Closed) ->
    maps:get(Goal, Closed, 'undefined'); % undefined if no way is found to goal
a_star(Nodes, Goal, [{{X, Y}, Distance} | Open], Closed) ->
    Height = maps:get({X, Y}, Nodes),

    Neighbors = [Neighbor
                 || Neighbor <- [{X-1, Y}, {X+1, Y}, {X, Y-1}, {X, Y+1}],
                    maps:is_key(Neighbor, Nodes), % is it a valid point
                    'false' =:= maps:is_key(Neighbor, Closed), % if not in closed already
                    maps:get(Neighbor, Nodes) =< Height+1 % is the height close enough
                ],

    {NewOpen, _} =
        lists:foldl(fun add_neighbor/2
                   ,{Open, Distance}
                   ,Neighbors
                   ),
    a_star(Nodes, Goal, lists:keysort(2, NewOpen), Closed#{{X, Y} => Distance}).

add_neighbor(Neighbor, {Open, Distance}) ->
    case lists:keyfind(Neighbor, 1, Open) of
        'false' ->
            {[{Neighbor, Distance+1} | Open], Distance};
        _ -> {Open, Distance}
    end.

part2(#graph{goal=Goal, nodes=Nodes}) ->
    [Start | StartingPoints] = maps:fold(fun starting_points/3, [], Nodes),
    LeastSteps =
        lists:foldl(fun(StartPoint, Steps) ->
                            case a_star(Nodes, Goal, StartPoint) of
                                'undefined' -> Steps;
                                Fewer when Fewer < Steps -> Fewer;
                                _More -> Steps
                            end
                    end
                   ,a_star(Nodes, Goal, Start)
                   ,StartingPoints
                   ),

    io:format("least steps of ~p~n", [LeastSteps]).

starting_points(XY, 0, Starts) ->
    [XY | Starts];
starting_points(_, _, Starts) ->
    Starts.

input() ->
    HeightMap = binary:split(input:input(<<?MODULE_STRING>>), <<$\n>>, ['global', 'trim']),
    parse_height_map(HeightMap).

parse_height_map(HM) ->
    parse_height_map(HM, 0, #graph{nodes=#{}}).

parse_height_map([], _MaxY, Graph) -> Graph;
parse_height_map([Line | Lines], Y, Graph) ->
    parse_height_map(Lines, Y+1, parse_row(Line, Y, Graph)).

parse_row(Line, Y, Graph) ->
    {_X, _Y, #graph{}=G} =
        lists:foldl(fun parse_height/2
                   ,{0, Y, Graph}
                   ,binary_to_list(Line)
                   ),
    G.

parse_height($S, {X, Y, #graph{nodes=Ns}=Graph}) ->
    {X+1, Y, Graph#graph{start={X, Y}, nodes=Ns#{{X, Y} => $a-$a}}};
parse_height($E, {X, Y, #graph{nodes=Ns}=Graph}) ->
    {X+1, Y, Graph#graph{goal={X, Y}, nodes=Ns#{{X, Y} => $z-$a}}};
parse_height(Height, {X, Y, #graph{nodes=Ns}=Graph}) ->
    {X+1, Y, Graph#graph{nodes=Ns#{{X, Y} => Height-$a}}}.
