#!/usr/bin/env escript
%%! +A0 -sname day4
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([]) ->
    {ok, Input} = file:read_file("puzzle.txt"),
    %% {'ok', Input} = file:read_file("sample.txt"),
    %% Input = <<"..X...\n.SAMX.\n.A..A.\nXMAS.S\n.X....\n......">>,

    Rows = binary:split(Input, <<"\n">>, ['global', 'trim']),
    {_Y, Map} = lists:foldl(fun to_map/2, {1, #{}}, Rows),

    [MaxXY | _] = lists:reverse(lists:sort(maps:keys(Map))),
    io:format("max XY: ~p~n", [MaxXY]),

    Count = count_xmas(Map, MaxXY),

    io:format("found: ~p~n", [Count]).

count_xmas(Map, MaxXY) ->
    RowCount = walk_rows(Map, MaxXY),
    ColCount = walk_cols(Map, MaxXY),
    L2RCount = walk_l2r(Map, MaxXY),
    R2LCount = walk_r2l(Map, MaxXY),

    RowCount + ColCount + L2RCount + R2LCount.


walk_l2r(Map, {_MaxX, MaxY}=MaxXY) ->
    walk_l2r(Map, MaxXY, {1, MaxY-3}, 0).

walk_l2r(_Map, {MaxX, _MaxY}, {X, 1}, Count) when X > MaxX-3 ->
    Count;

%% just processed {1,1}->MaxXY, incr X now
walk_l2r(Map, MaxXY, {1, 0}, Count) ->
    walk_l2r(Map, MaxXY, {2, 1}, Count);

%% left side of diaganol
walk_l2r(Map, {MaxX, MaxY}, {1, Y}, Count) ->
    Xs = lists:seq(1, 1+(MaxY-Y)),
    Ys = lists:seq(Y, MaxY),

    DiagRow = lists:zipwith(fun(L, R) -> maps:get({L, R}, Map) end
                           ,Xs
                           ,Ys
                           ),
    walk_l2r(Map, {MaxX, MaxY}, {1, Y-1}, Count + count_diag(DiagRow));

%% right side of diaganol
walk_l2r(Map, {MaxX, MaxY}, {X, 1}, Count) ->
    Xs = lists:seq(X, MaxX),
    Ys = lists:seq(1, (MaxX-X)+1),

    DiagRow = lists:zipwith(fun(L, R) -> maps:get({L, R}, Map) end
                           ,Xs
                           ,Ys
                           ),
    walk_l2r(Map, {MaxX, MaxY}, {X+1, 1}, Count + count_diag(DiagRow)).


walk_r2l(Map, {MaxX, MaxY}=MaxXY) ->
    %% start under mid diaganol
    walk_r2l(Map, MaxXY, {MaxX, MaxY-3}, 0).

%% finished right side of R2L diaganol
walk_r2l(Map, {MaxX, _}=MaxXY, {_, 0}, Count) ->
    walk_r2l(Map, MaxXY, {MaxX-1, 1}, Count);
walk_r2l(_Map, {_MaxX, _MaxY}, {X, _Y}, Count) when X < 4 ->
    Count;

%% right side of diaganol
%% {6,3},{5,4},{4,5},{3,6}
%% {6,2},{5,3},{4,4},{3,5},{2,6}
%% {6,1},{5,2},{4,3},{3,4},{2,5},{1,6}
walk_r2l(Map, {MaxX, MaxY}, {MaxX, Y}, Count) ->
    Ys = lists:seq(Y, MaxX),
    Xs = lists:seq(MaxX, Y, -1),

    DiagRow = lists:zipwith(fun(L, R) -> maps:get({L, R}, Map) end
                           ,Xs
                           ,Ys
                           ),

    walk_r2l(Map, {MaxX, MaxY}, {MaxX, Y-1}, Count + count_diag(DiagRow));

%% left side of diaganol
%% {5,1},{4,2},{3,3},{2,4},{1,5}
%% {4,1},{3,2},{2,3},{1,4}
%% {3,1},{2,2},{1,3}
walk_r2l(Map, {MaxX, MaxY}, {X, 1}, Count) ->
    Ys = lists:seq(1, X),
    Xs = lists:seq(X, 1, -1),

    DiagRow = lists:zipwith(fun(L, R) -> maps:get({L, R}, Map) end
                           ,Xs
                           ,Ys
                           ),
    walk_r2l(Map, {MaxX, MaxY}, {X-1, 1}, Count + count_diag(DiagRow)).

count_diag(Row) -> count_diag(Row, 0).

count_diag([A, B, C, D | Rest], Count) ->
    count_diag([B, C, D | Rest], Count + maybe_incr([A, B, C, D]));
count_diag(_, Count) -> Count.

walk_rows(Map, MaxXY) ->
    walk_rows(Map, MaxXY, {1, 1}, 0).

walk_rows(_Map, {MaxX, MaxY}, {X, MaxY}, Count) when X =:= MaxX-2 -> Count;
walk_rows(Map, {MaxX, MaxY}, {X, Y}, Count) when X =:= MaxX-2 ->
    walk_rows(Map, {MaxX, MaxY}, {1, Y+1}, Count);
walk_rows(Map, MaxXY, {X, Y}, Count) ->
    XMAS = [maps:get({RX, Y}, Map) || RX <- lists:seq(X, X+3)],
    Inc = maybe_incr(XMAS),
    walk_rows(Map, MaxXY, {X+1, Y}, Count + Inc).

maybe_incr("XMAS") -> 1;
maybe_incr("SAMX") -> 1;
maybe_incr(_) -> 0.

walk_cols(Map, MaxXY) ->
    walk_cols(Map, MaxXY, {1, 1}, 0).

walk_cols(_Map, {MaxX, MaxY}, {MaxX, Y}, Count) when Y =:= MaxY-2 -> Count;
walk_cols(Map, {MaxX, MaxY}, {X, Y}, Count) when Y =:= MaxY-2 ->
    walk_cols(Map, {MaxX, MaxY}, {X+1, 1}, Count);
walk_cols(Map, MaxXY, {X, Y}, Count) ->
    XMAS = [maps:get({X, RY}, Map) || RY <- lists:seq(Y, Y+3)],
    Inc = maybe_incr(XMAS),
    walk_cols(Map, MaxXY, {X, Y+1}, Count + Inc).

to_map(Row, {Y, Map}) ->
    {_, _, Map1} = lists:foldl(fun to_cell/2, {1, Y, Map}, binary_to_list(Row)),
    {Y+1, Map1}.

to_cell(Cell, {X, Y, Map}) ->
    {X+1, Y, Map#{{X, Y} => Cell}}.
