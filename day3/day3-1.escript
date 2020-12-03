#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 3: Toboggan Trajectory ---

%% With the toboggan login problems resolved, you set off toward the
%% airport. While travel by toboggan might be easy, it's certainly not
%% safe: there's very minimal steering and the area is covered in
%% trees. You'll need to see which angles will take you near the
%% fewest trees.

%% Due to the local geology, trees in this area only grow on exact
%% integer coordinates in a grid. You make a map (your puzzle input)
%% of the open squares (.) and trees (#) you can see. For example:

%% ..##.......
%% #...#...#..
%% .#....#..#.
%% ..#.#...#.#
%% .#...##..#.
%% ..#.##.....
%% .#.#.#....#
%% .#........#
%% #.##...#...
%% #...##....#
%% .#..#...#.#

%% These aren't the only trees, though; due to something you read
%% about once involving arboreal genetics and biome stability, the
%% same pattern repeats to the right many times:

%% ..##.........##.........##.........##.........##.........##.......  --->
%% #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
%% .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
%% ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
%% .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
%% ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
%% .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
%% .#........#.#........#.#........#.#........#.#........#.#........#
%% #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
%% #...##....##...##....##...##....##...##....##...##....##...##....#
%% .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

%% You start on the open square (.) in the top-left corner and need to
%% reach the bottom (below the bottom-most row on your map).

%% The toboggan can only follow a few specific slopes (you opted for a
%% cheaper model that prefers rational numbers); start by counting all
%% the trees you would encounter for the slope right 3, down 1:

%% From your starting position at the top-left, check the position
%% that is right 3 and down 1. Then, check the position that is right
%% 3 and down 1 from there, and so on until you go past the bottom of
%% the map.

%% The locations you'd check in the above example are marked here with
%% O where there was an open square and X where there was a tree:

%% ..##.........##.........##.........##.........##.........##.......  --->
%% #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
%% .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
%% ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
%% .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
%% ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
%% .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
%% .#........#.#........X.#........#.#........#.#........#.#........#
%% #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
%% #...##....##...##....##...#X....##...##....##...##....##...##....#
%% .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

%% In this example, traversing the map using this slope would cause
%% you to encounter 7 trees.

%% Starting at the top-left corner of your map and following a slope
%% of right 3 and down 1, how many trees would you encounter?

-define(TREE, $#).
-define(OPEN, $.).

-record(graph, {max_x = 1, max_y = 1, locations = #{}}).

main(_) ->
    #graph{max_x=_X, max_y=_Y}=Graph = read_input("p3.txt"),
    %% io:format("max {~p, ~p}~n", [_X, _Y]),
    TreesHit = count_trees_in_path(Graph),
    io:format("trees: ~p~n", [TreesHit]).

count_trees_in_path(Graph) ->
    count_trees_in_path(Graph, move_sled({1, 1}), 0).

count_trees_in_path(#graph{max_y=MaxY}, {_X, MaxY}, TreesHit) -> TreesHit;
count_trees_in_path(Graph, XY, TreesHit) ->
    case obstacle_at(Graph, XY) of
        ?TREE ->
            %% io:format("at ~p: tree~n", [XY]),
            count_trees_in_path(Graph, move_sled(XY), TreesHit+1);
        ?OPEN ->
            %% io:format("at ~p: open~n", [XY]),
            count_trees_in_path(Graph, move_sled(XY), TreesHit)
    end.

move_sled({X, Y}) ->
    {X+3, Y+1}.

obstacle_at(#graph{max_x=MaxX}=Graph, {X, Y}) when X > MaxX ->
    %% io:format("fixing {~p, ~p} to {~p, ~p}~n", [X, Y, X rem MaxX, Y]),
    obstacle_at(Graph, {X rem MaxX, Y});
obstacle_at(#graph{max_x=MaxX}=Graph, {0, Y}) ->
    %% io:format("fixing {0, ~p} to {~p, ~p}~n", [Y, MaxX, Y]),
    obstacle_at(Graph, {MaxX, Y});
obstacle_at(#graph{locations=Locations}, XY) ->
    maps:get(XY, Locations).

read_input(File) ->
    {'ok', Map} = file:read_file(File),
    Lines = binary:split(Map, <<"\n">>, ['global']),
    build_graph(Lines, 1, #graph{}).

build_graph([], MaxY, Graph) ->
    Graph#graph{max_y=MaxY};
build_graph([<<>>|Lines], Y, Graph) ->
    build_graph(Lines, Y, Graph);
build_graph([Line|Lines], Y, Graph) ->
    NewGraph = add_line_to_graph(Line, Y, Graph),
    build_graph(Lines, Y+1, NewGraph).

add_line_to_graph(Line, Y, Graph) ->
    {MaxX, _Y, NewGraph} = lists:foldl(fun add_location/2
                                      ,{1, Y, Graph}
                                      ,binary_to_list(Line)
                                      ),
    NewGraph#graph{max_x=MaxX-1}.

add_location(Obstacle, {X, Y, #graph{locations=Locations}=Graph}) ->
    {X+1, Y, Graph#graph{locations = Locations#{{X,Y} => Obstacle}}}.

%% |  X | Adj | REM 11 |
%% |  1 |  1  | 1
%% |  2 |  2  | 2
%% | 11 | 11  | 0
%% | 12 |  1  | 1
%% | 13 |  2  | 2
%% | 22 | 11  | 0
%% | 23 |  1  | 1
%% | 24 |  2  | 2
