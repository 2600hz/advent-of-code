#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 11: Seating System ---

%% Your plane lands with plenty of time to spare. The final leg of your
%% journey is a ferry that goes directly to the tropical island where
%% you can finally start your vacation. As you reach the waiting area
%% to board the ferry, you realize you're so early, nobody else has
%% even arrived yet!

%% By modeling the process people use to choose (or abandon) their seat
%% in the waiting area, you're pretty sure you can predict the best
%% place to sit. You make a quick map of the seat layout (your puzzle
%% input).

%% The seat layout fits neatly on a grid. Each position is either floor
%% (.), an empty seat (L), or an occupied seat (#). For example, the
%% initial seat layout might look like this:

%% L.LL.LL.LL
%% LLLLLLL.LL
%% L.L.L..L..
%% LLLL.LL.LL
%% L.LL.LL.LL
%% L.LLLLL.LL
%% ..L.L.....
%% LLLLLLLLLL
%% L.LLLLLL.L
%% L.LLLLL.LL

%% Now, you just need to model the people who will be arriving
%% shortly. Fortunately, people are entirely predictable and always
%% follow a simple set of rules. All decisions are based on the number
%% of occupied seats adjacent to a given seat (one of the eight
%% positions immediately up, down, left, right, or diagonal from the
%% seat). The following rules are applied to every seat simultaneously:

%%     If a seat is empty (L) and there are no occupied seats adjacent
%%     to it, the seat becomes occupied.
%%     If a seat is occupied (#) and four or more seats adjacent to it
%%     are also occupied, the seat becomes empty.
%%     Otherwise, the seat's state does not change.

%% Floor (.) never changes; seats don't move, and nobody sits on the
%% floor.

%% After one round of these rules, every seat in the example layout
%% becomes occupied:

%% #.##.##.##
%% #######.##
%% #.#.#..#..
%% ####.##.##
%% #.##.##.##
%% #.#####.##
%% ..#.#.....
%% ##########
%% #.######.#
%% #.#####.##

%% After a second round, the seats with four or more occupied adjacent
%% seats become empty again:

%% #.LL.L#.##
%% #LLLLLL.L#
%% L.L.L..L..
%% #LLL.LL.L#
%% #.LL.LL.LL
%% #.LLLL#.##
%% ..L.L.....
%% #LLLLLLLL#
%% #.LLLLLL.L
%% #.#LLLL.##

%% This process continues for three more rounds:

%% #.##.L#.##
%% #L###LL.L#
%% L.#.#..#..
%% #L##.##.L#
%% #.##.LL.LL
%% #.###L#.##
%% ..#.#.....
%% #L######L#
%% #.LL###L.L
%% #.#L###.##

%% #.#L.L#.##
%% #LLL#LL.L#
%% L.L.L..#..
%% #LLL.##.L#
%% #.LL.LL.LL
%% #.LL#L#.##
%% ..L.L.....
%% #L#LLLL#L#
%% #.LLLLLL.L
%% #.#L#L#.##

%% #.#L.L#.##
%% #LLL#LL.L#
%% L.#.L..#..
%% #L##.##.L#
%% #.#L.LL.LL
%% #.#L#L#.##
%% ..L.L.....
%% #L#L##L#L#
%% #.LLLLLL.L
%% #.#L#L#.##

%% At this point, something interesting happens: the chaos stabilizes
%% and further applications of these rules cause no seats to change
%% state! Once people stop moving around, you count 37 occupied seats.

%% Simulate your seating area by applying the seating rules repeatedly
%% until no seats change state. How many seats end up occupied?

-define(FLOOR, $.).
-define(EMPTY, $L).
-define(TAKEN, $#).

main(_) ->
    FloorPlan = read_input("p11.txt"),
    io:format("starting floor plan:~n"),
    print_floor_plan(FloorPlan),

    SteadyFloorPlan = run_until_steady(FloorPlan),
    io:format("steady:~n"),
    print_floor_plan(SteadyFloorPlan),

    Taken = lists:sum([1 || ?TAKEN <- maps:values(SteadyFloorPlan)]),
    io:format("taken: ~p~n", [Taken]).

run_until_steady(FloorPlan) ->
    case adjust_floor_plan(FloorPlan) of
        {0, SteadyFloorPlan} -> SteadyFloorPlan;
        {_Adjusted, NextFloorPlan} -> run_until_steady(NextFloorPlan)
    end.

adjust_floor_plan(FloorPlan) ->
    {Adjustments, _FP, Next} = maps:fold(fun adjust_seat/3, {0, FloorPlan, #{}}, FloorPlan),
    {Adjustments, Next}.

%%     If a seat is empty (L) and there are no occupied seats adjacent
%%     to it, the seat becomes occupied.
%%     If a seat is occupied (#) and four or more seats adjacent to it
%%     are also occupied, the seat becomes empty.
%%     Otherwise, the seat's state does not change.
%% Floor (.) never changes; seats don't move, and nobody sits on the
%% floor.
adjust_seat(XY, ?FLOOR, {Ads, Prev, Next}) ->
    {Ads, Prev, Next#{XY => ?FLOOR}};
adjust_seat(SeatXY, ?EMPTY, {Ads, Prev, Next}) ->
    case occupied_adjacent(SeatXY, Prev) of
        [] -> {Ads+1, Prev, Next#{SeatXY => ?TAKEN}};
        _ -> {Ads, Prev, Next#{SeatXY => ?EMPTY}}
    end;
adjust_seat(SeatXY, ?TAKEN, {Ads, Prev, Next}) ->
    case occupied_adjacent(SeatXY, Prev) of
        [_, _, _, _ | _] -> {Ads+1, Prev, Next#{SeatXY => ?EMPTY}};
        _ -> {Ads, Prev, Next#{SeatXY => ?TAKEN}}
    end.

occupied_adjacent({SeatX, SeatY}, FloorPlan) ->
    [{X, Y} || X <- lists:seq(SeatX-1, SeatX+1),
              Y <- lists:seq(SeatY-1, SeatY+1),
              SeatStatus <- [maps:get({X, Y}, FloorPlan, 'undefined')],
              {X, Y} =/= {SeatX, SeatY} andalso SeatStatus =:= ?TAKEN
    ].

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    {_, FloorPlan} =
        lists:foldl(fun line_to_floor_plan/2
                   ,{1, #{}}
                   ,binary:split(Lines, <<"\n">>, ['global', 'trim'])
                   ),
    FloorPlan.

line_to_floor_plan(Line, {Y, FloorPlan}) ->
    {_X, Y, NextFloorPlan} =
        lists:foldl(fun seat_status/2
                   ,{1, Y, FloorPlan}
                   ,binary_to_list(Line)
                   ),
    {Y+1, NextFloorPlan}.

seat_status(SeatStatus, {X, Y, FloorPlan}) ->
    {X+1, Y, FloorPlan#{{X, Y} => SeatStatus}}.

print_floor_plan(FloorPlan) ->
    print_floor_plan(FloorPlan, {1, 1}).

print_floor_plan(FloorPlan, {X, Y}) ->
    case maps:get({X, Y}, FloorPlan, 'undefined') of
        'undefined' ->
            io:format("~n"),
            NextXY = {1, Y+1},
            case maps:get(NextXY, FloorPlan, 'undefined') of
                'undefined' -> io:format("~n");
                _ -> print_floor_plan(FloorPlan, NextXY)
            end;
        Seat ->
            io:format("~s", [[Seat]]),
            print_floor_plan(FloorPlan, {X+1, Y})
    end.
