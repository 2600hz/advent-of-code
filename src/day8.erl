-module(day8).

%% --- Day 8: Treetop Tree House ---

%% The expedition comes across a peculiar patch of tall trees all
%% planted carefully in a grid. The Elves explain that a previous
%% expedition planted these trees as a reforestation effort. Now,
%% they're curious if this would be a good location for a tree house.

%% First, determine whether there is enough tree cover here to keep a
%% tree house hidden. To do this, you need to count the number of
%% trees that are visible from outside the grid when looking directly
%% along a row or column.

%% The Elves have already launched a quadcopter to generate a map with
%% the height of each tree (your puzzle input). For example:

%% 30373
%% 25512
%% 65332
%% 33549
%% 35390

%% Each tree is represented as a single digit whose value is its
%% height, where 0 is the shortest and 9 is the tallest.

%% A tree is visible if all of the other trees between it and an edge
%% of the grid are shorter than it. Only consider trees in the same
%% row or column; that is, only look up, down, left, or right from any
%% given tree.

%% All of the trees around the edge of the grid are visible - since
%% they are already on the edge, there are no trees to block the
%% view. In this example, that only leaves the interior nine trees to
%% consider:

%%     The top-left 5 is visible from the left and top. (It isn't
%%     visible from the right or bottom since other trees of height 5
%%     are in the way.)

%%     The top-middle 5 is visible from the top and right.

%%     The top-right 1 is not visible from any direction; for it to be
%%     visible, there would need to only be trees of height 0 between
%%     it and an edge.

%%     The left-middle 5 is visible, but only from the right.

%%     The center 3 is not visible from any direction; for it to be
%%     visible, there would need to be only trees of at most height 2
%%     between it and an edge.

%%     The right-middle 3 is visible from the right.

%%     In the bottom row, the middle 5 is visible, but the 3 and 4 are
%%     not.

%% With 16 trees visible on the edge and another 5 visible in the
%% interior, a total of 21 trees are visible in this arrangement.

%% Consider your map; how many trees are visible from outside the
%% grid?

%% --- Part Two ---

%% Content with the amount of tree cover available, the Elves just
%% need to know the best spot to build their tree house: they would
%% like to be able to see a lot of trees.

%% To measure the viewing distance from a given tree, look up, down,
%% left, and right from that tree; stop if you reach an edge or at the
%% first tree that is the same height or taller than the tree under
%% consideration. (If a tree is right on the edge, at least one of its
%% viewing distances will be zero.)

%% The Elves don't care about distant trees taller than those found by
%% the rules above; the proposed tree house has large eaves to keep it
%% dry, so they wouldn't be able to see higher than the tree house
%% anyway.

%% In the example above, consider the middle 5 in the second row:

%% 30373
%% 25512
%% 65332
%% 33549
%% 35390

%%     Looking up, its view is not blocked; it can see 1 tree (of
%%     height 3).

%%     Looking left, its view is blocked immediately; it can see only
%%     1 tree (of height 5, right next to it).

%%     Looking right, its view is not blocked; it can see 2 trees.

%%     Looking down, its view is blocked eventually; it can see 2
%%     trees (one of height 3, then the tree of height 5 that blocks
%%     its view).

%% A tree's scenic score is found by multiplying together its viewing
%% distance in each of the four directions. For this tree, this is 4
%% (found by multiplying 1 * 1 * 2 * 2).

%% However, you can do even better: consider the tree of height 5 in
%% the middle of the fourth row:

%% 30373
%% 25512
%% 65332
%% 33549
%% 35390

%%     Looking up, its view is blocked at 2 trees (by another tree
%%     with a height of 5).

%%     Looking left, its view is not blocked; it can see 2 trees.

%%     Looking down, its view is also not blocked; it can see 1 tree.

%%     Looking right, its view is blocked at 2 trees (by a massive
%%     tree of height 9).

%% This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal
%% spot for the tree house.

%% Consider each tree on your map. What is the highest scenic score
%% possible for any tree?

-export([run/0
        ,part1/0
        ,part2/0
        ]).

run() ->
    Input = input(),
    part1(Input),
    part2(Input).

part1() ->
    part1(input()).

part2() ->
    part2(input()).

part1({_MaxX, _MaxY, Grid}=State) ->
    {_, Visible} = maps:fold(fun count_visible/3, {State, 0}, Grid),
    io:format("there are ~p visible trees~n", [Visible]).

count_visible(TreeXY, TreeHeight, {State, Visible}) ->
    case is_visible(State, TreeXY, TreeHeight) of
        'true' -> {State, Visible+1};
        'false' -> {State, Visible}
    end.

is_visible(State, TreeXY, TreeHeight) ->
    is_visible_row(State, TreeXY, TreeHeight)
        orelse is_visible_column(State, TreeXY, TreeHeight).

is_visible_row({_MaxX, _MaxY, _Grid}, {0, _Y}, _Height) -> 'true';
is_visible_row({MaxX, _MaxY, _Grid}, {MaxX, _Y}, _Height) -> 'true';
is_visible_row({MaxX, _MaxY, Grid}, {X, Y}, TreeHeight) ->
    LeftOf =  [{L, Y} || L <- lists:seq(0, X-1)],
    RightOf = [{R, Y} || R <- lists:seq(X+1, MaxX)],

    is_visible_tree(Grid, TreeHeight, LeftOf)
        orelse is_visible_tree(Grid, TreeHeight, RightOf).

is_visible_column({_MaxX, _MaxY, _Grid}, {_X, 0}, _Height) -> 'true';
is_visible_column({_MaxX, MaxY, _Grid}, {_X, MaxY}, _Height) -> 'true';
is_visible_column({_MaxX, MaxY, Grid}, {X, Y}, TreeHeight) ->
    Below = [{X, L} || L <- lists:seq(0, Y-1)],
    Above = [{X, R} || R <- lists:seq(Y+1, MaxY)],

    is_visible_tree(Grid, TreeHeight, Below)
        orelse is_visible_tree(Grid, TreeHeight, Above).


is_visible_tree(Grid, TreeHeight, TreesToEdge) ->
    lists:all(fun(TreeXY) -> maps:get(TreeXY, Grid) < TreeHeight end
             ,TreesToEdge
             ).

part2({_MaxX, _MaxY, Grid}=State) ->
    {_, MaxScenicScore} = maps:fold(fun scenic_score_fold/3, {State, 0}, Grid),
    io:format("max scenic score: ~p~n", [MaxScenicScore]).

scenic_score_fold({X, Y}, TreeHeight, {State, MaxScore}) ->
    case scenic_score(State, TreeHeight, {X, Y}) of
        Score when Score > MaxScore ->
            {State, Score};
        _Score ->
            {State, MaxScore}
    end.

scenic_score({MaxX, MaxY, Grid}, TreeHeight, {X, Y}) ->
    AboveScore = tree_score(Grid, TreeHeight, [{X, AboveY} || AboveY <- lists:seq(Y-1, 0, -1)]),
    BelowScore = tree_score(Grid, TreeHeight, [{X, BelowY} || BelowY <- lists:seq(Y+1, MaxY)]),

    LeftScore = tree_score(Grid, TreeHeight, [{LeftX, Y} || LeftX <- lists:seq(X-1, 0, -1)]),
    RightScore = tree_score(Grid, TreeHeight, [{RightX, Y} || RightX <- lists:seq(X+1, MaxX)]),

    AboveScore * BelowScore * LeftScore * RightScore.

tree_score(Grid, TreeHeight, Trees) ->
    tree_score(Grid, TreeHeight, Trees, 0).

tree_score(_Grid, _TreeHeight, [], Count) -> Count;
tree_score(Grid, TreeHeight, [TreeXY | Trees], Count) ->
    case maps:get(TreeXY, Grid) of
        Height when Height < TreeHeight ->
            tree_score(Grid, TreeHeight, Trees, Count+1); % tree not blocking our tree
        _Height ->
            Count+1 % blocked by this tree
    end.

input() ->
    Input = input:input(<<?MODULE_STRING>>),
    Bins = binary:split(Input, <<$\n>>, ['global', 'trim']),
    parse_tree_heights(Bins).

parse_tree_heights([_|_]=Bins) ->
    arrange_grid([parse_tree_heights(Bin) || Bin <- Bins]);
parse_tree_heights(<<Bin/binary>>) ->
    [Ascii-$0 || <<Ascii>> <= Bin].

arrange_grid(Heights) ->
    arrange_grid(Heights, 0, 0, #{}).

arrange_grid([], MaxY, MaxX, Grid) -> {MaxX-1, MaxY-1, Grid};
arrange_grid([Height | Heights], Y, 0, Grid) ->
    {MaxX, NewGrid} = arrange_row(Height, Y, 0, Grid),
    arrange_grid(Heights, Y+1, MaxX, NewGrid);
arrange_grid([Height | Heights], Y, MaxX, Grid) ->
    {_, NewGrid} = arrange_row(Height, Y, 0, Grid),
    arrange_grid(Heights, Y+1, MaxX, NewGrid).

arrange_row([], _Y, X, Grid) -> {X, Grid};
arrange_row([Tree | Trees], Y, X, Grid) ->
    arrange_row(Trees, Y, X+1, Grid#{{X, Y} => Tree}).
