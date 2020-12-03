#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% Time to check the rest of the slopes - you need to minimize the
%% probability of a sudden arboreal stop, after all.

%% Determine the number of trees you would encounter if, for each of
%% the following slopes, you start at the top-left corner and traverse
%% the map all the way to the bottom:

%%     Right 1, down 1.
%%     Right 3, down 1. (This is the slope you already checked.)
%%     Right 5, down 1.
%%     Right 7, down 1.
%%     Right 1, down 2.

%% In the above example, these slopes would find 2, 7, 3, 4, and 2
%% tree(s) respectively; multiplied together, these produce the answer
%% 336.

%% What do you get if you multiply together the number of trees
%% encountered on each of the listed slopes?


-define(TREE, $#).
-define(OPEN, $.).

-record(graph, {max_x = 1, max_y = 1, locations = #{}}).

main(_) ->
    #graph{max_x=_X, max_y=_Y}=Graph = read_input("p3.txt"),
    SledMovers = [fun move_one_by_one/1
                 ,fun move_three_by_one/1
                 ,fun move_five_by_one/1
                 ,fun move_seven_by_one/1
                 ,fun move_one_by_two/1
                 ],
    %% io:format("max {~p, ~p}~n", [_X, _Y]),
    TreesHit = [count_trees_in_path(Graph, SledMover, {1, 1})
                || SledMover <- SledMovers
               ],
    Product = lists:foldl(fun erlang:'*'/2, 1, TreesHit),
    io:format("trees: ~p: ~p~n", [Product, TreesHit]).

count_trees_in_path(Graph, SledMover, SledPos) ->
    count_trees_in_path(Graph, SledMover, SledMover(SledPos), 0).

count_trees_in_path(#graph{max_y=MaxY}, _SledMover, {_X, Y}, TreesHit) when Y >= MaxY ->
    TreesHit;
count_trees_in_path(Graph, SledMover, XY, TreesHit) ->
    case obstacle_at(Graph, XY) of
        ?TREE ->
            %% io:format("at ~p: tree~n", [XY]),
            count_trees_in_path(Graph, SledMover, SledMover(XY), TreesHit+1);
        ?OPEN ->
            %% io:format("at ~p: open~n", [XY]),
            count_trees_in_path(Graph, SledMover, SledMover(XY), TreesHit)
    end.

move_one_by_one({X, Y}) ->
    {X+1, Y+1}.

move_three_by_one({X, Y}) ->
    {X+3, Y+1}.

move_five_by_one({X, Y}) ->
    {X+5, Y+1}.

move_seven_by_one({X, Y}) ->
    {X+7, Y+1}.

move_one_by_two({X, Y}) ->
    {X+1, Y+2}.

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
