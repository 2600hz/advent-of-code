#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% Ding! The "fasten seat belt" signs have turned on. Time to find your
%% seat.

%% It's a completely full flight, so your seat should be the only
%% missing boarding pass in your list. However, there's a catch: some
%% of the seats at the very front and back of the plane don't exist on
%% this aircraft, so they'll be missing from your list as well.

%% Your seat wasn't at the very front or back, though; the seats with
%% IDs +1 and -1 from yours will be in your list.

%% What is the ID of your seat?

-define(BACK, $B).
-define(FRONT, $F).
-define(LEFT, $L).
-define(RIGHT, $R).

main(_) ->
    Passes = read_input("p5.txt"),
    {SeatId, _R, _C} = find_missing_pass(Passes),
    io:format("seat ~p: row ~p col ~p~n", [SeatId, _R, _C]).

find_missing_pass(Passes) ->
    find_missing_pass(Passes, maps:keys(Passes)).

find_missing_pass(Passes, [Row | Rows]) ->
    find_missing_pass(Passes, Rows, Row).

find_missing_pass(Passes, Rows, Row) ->
    Cols = maps:get(Row, Passes),
    case length(Cols) of
        8 -> find_missing_pass(Passes, Rows);
        7 ->
            Col = find_gap(lists:keysort(1, Cols)),
            {Row * 8 + Col, Row, Col}
    end.

find_gap([{L, _LS}, {R, _RS} | _]) when R =:= L+2 ->
    L+1;
find_gap([_ | Cols]) -> find_gap(Cols).

read_input(File) ->
    {'ok', Map} = file:read_file(File),
    lists:foldl(fun process_boarding_pass/2
               ,#{}
               ,binary:split(Map, <<"\n">>, ['global'])
               ).

process_boarding_pass(<<>>, Passes) -> Passes;
process_boarding_pass(<<FrontBack:7/binary, LeftRight:3/binary>>, Passes) ->
    process_boarding_pass(to_row(FrontBack), to_col(LeftRight), Passes).

process_boarding_pass(1, _, Passes) ->
    Passes;
process_boarding_pass(128, _, Passes) ->
    Passes;
process_boarding_pass(Row, Col, Passes) ->
    SeatId = Row * 8 + Col,
    %% io:format("~p: row ~s ~p: col: ~s ~p~n", [SeatId, FrontBack, Row, LeftRight, Col]),

    Cols = maps:get(Row, Passes, []),
    Passes#{Row => [{Col, SeatId} | Cols]}.

to_row(FrontBack) ->
    binary_space_partition(FrontBack, ?FRONT, ?BACK).

to_col(LeftRight) ->
    binary_space_partition(LeftRight, ?LEFT, ?RIGHT).

binary_space_partition(Bin, Lower, Higher) ->
    binary_space_partition(Bin, Lower, Higher, 0, round(math:pow(2, byte_size(Bin)))-1).

binary_space_partition(<<Lower:8>>, Lower, _Upper, LowBound, _HighBound) ->
    %% io:format("  using lower bound ~p~n", [LowBound]),
    LowBound;
binary_space_partition(<<Upper:8>>, _Lower, Upper, _LowBound, HighBound) ->
    %% io:format("  using higher bound ~p~n", [HighBound]),
    HighBound;
binary_space_partition(<<Lower:8, Rest/binary>>, Lower, Upper, LowBound, HighBound) ->
    Diff = (HighBound-LowBound) div 2,
    %% io:format("  updating bounds to ~p -> ~p~n", [LowBound, LowBound + Diff]),
    binary_space_partition(Rest, Lower, Upper, LowBound, LowBound + Diff);
binary_space_partition(<<Upper:8, Rest/binary>>, Lower, Upper, LowBound, HighBound) ->
    Diff = (HighBound-LowBound) div 2,
    %% io:format("  updating bounds to ~p -> ~p~n", [HighBound-Diff, HighBound]),
    binary_space_partition(Rest, Lower, Upper, HighBound - Diff, HighBound).
