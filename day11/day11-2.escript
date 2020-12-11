#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% As soon as people start to arrive, you realize your mistake. People
%% don't just care about adjacent seats - they care about the first
%% seat they can see in each of those eight directions!

%% Now, instead of considering just the eight immediately adjacent
%% seats, consider the first seat in each of those eight
%% directions. For example, the empty seat below would see eight
%% occupied seats:

%% .......#.
%% ...#.....
%% .#.......
%% .........
%% ..#L....#
%% ....#....
%% .........
%% #........
%% ...#.....

%% The leftmost empty seat below would only see one empty seat, but
%% cannot see any of the occupied ones:

%% .............
%% .L.L.#.#.#.#.
%% .............

%% The empty seat below would see no occupied seats:

%% .##.##.
%% #.#.#.#
%% ##...##
%% ...L...
%% ##...##
%% #.#.#.#
%% .##.##.

%% Also, people seem to be more tolerant than you expected: it now
%% takes five or more visible occupied seats for an occupied seat to
%% become empty (rather than four or more from the previous rules). The
%% other rules still apply: empty seats that see no occupied seats
%% become occupied, seats matching no rule don't change, and floor
%% never changes.

%% Given the same starting layout as above, these new rules cause the
%% seating area to shift around as follows:

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

%% #.LL.LL.L#
%% #LLLLLL.LL
%% L.L.L..L..
%% LLLL.LL.LL
%% L.LL.LL.LL
%% L.LLLLL.LL
%% ..L.L.....
%% LLLLLLLLL#
%% #.LLLLLL.L
%% #.LLLLL.L#

%% #.L#.##.L#
%% #L#####.LL
%% L.#.#..#..
%% ##L#.##.##
%% #.##.#L.##
%% #.#####.#L
%% ..#.#.....
%% LLL####LL#
%% #.L#####.L
%% #.L####.L#

%% #.L#.L#.L#
%% #LLLLLL.LL
%% L.L.L..#..
%% ##LL.LL.L#
%% L.LL.LL.L#
%% #.LLLLL.LL
%% ..L.L.....
%% LLLLLLLLL#
%% #.LLLLL#.L
%% #.L#LL#.L#

%% #.L#.L#.L#
%% #LLLLLL.LL
%% L.L.L..#..
%% ##L#.#L.L#
%% L.L#.#L.L#
%% #.L####.LL
%% ..#.#.....
%% LLL###LLL#
%% #.LLLLL#.L
%% #.L#LL#.L#

%% #.L#.L#.L#
%% #LLLLLL.LL
%% L.L.L..#..
%% ##L#.#L.L#
%% L.L#.LL.L#
%% #.LLLL#.LL
%% ..#.L.....
%% LLL###LLL#
%% #.LLLLL#.L
%% #.L#LL#.L#

%% Again, at this point, people stop shifting around and the seating
%% area reaches equilibrium. Once this occurs, you count 26 occupied
%% seats.

%% Given the new visibility method and the rule change for occupied
%% seats becoming empty, once equilibrium is reached, how many seats
%% end up occupied?

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
    %% io:format("adj ~p~n", [Adjustments]),
    %% print_floor_plan(Next),
    {Adjustments, Next}.

%%     If a seat is empty (L) and there are no occupied seats adjacent
%%     to it, the seat becomes occupied.
%%     If a seat is occupied (#) and five or more seats adjacent to it
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
        [_, _, _, _, _ | _] -> {Ads+1, Prev, Next#{SeatXY => ?EMPTY}};
        _ -> {Ads, Prev, Next#{SeatXY => ?TAKEN}}
    end.

%% Twist: not adjacent but in line of sight
occupied_adjacent(SeatXY, FloorPlan) ->
    Directions = [{-1, -1}, {0, -1}, {1, -1} %% TopLeft Top TopRight
                 ,{-1, 0},           {1, 0}  %% Left    XY  Right
                 ,{-1, 1}, {0, 1}, {1, 1}    %% BotLeft Bot BotRight
                 ],

    {_, _, Occupied} =
        lists:foldl(fun find_seat_status/2
                   ,{SeatXY, FloorPlan, []}
                   ,Directions
                   ),
    Occupied.

find_seat_status(Adjustment, {SeatXY, FloorPlan, Occupied}) ->
    case find_seat_status(Adjustment, SeatXY, FloorPlan) of
        {XY, ?TAKEN} -> {SeatXY, FloorPlan, [XY | Occupied]};
        {_, _} -> {SeatXY, FloorPlan, Occupied}
    end.

find_seat_status({AX, AY}, {SX, SY}, FloorPlan) ->
    NewXY = {SX+AX, SY+AY},
    case maps:get(NewXY, FloorPlan, 'undefined') of
        'undefined' -> {NewXY, ?FLOOR};
        ?FLOOR -> find_seat_status({AX, AY}, NewXY, FloorPlan);
        Status -> {NewXY, Status}
    end.

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
