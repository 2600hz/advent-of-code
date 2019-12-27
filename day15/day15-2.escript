#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% You quickly repair the oxygen system; oxygen gradually fills the
%% area.

%% Oxygen starts in the location containing the repaired oxygen
%% system. It takes one minute for oxygen to spread to all open
%% locations that are adjacent to a location that already contains
%% oxygen. Diagonal locations are not adjacent.

%% In the example above, suppose you've used the droid to explore the
%% area fully and have the following map (where locations that
%% currently contain oxygen are marked O):

%%  ##
%% #..##
%% #.#..#
%% #.O.#
%%  ###

%% Initially, the only location which contains oxygen is the location
%% of the repaired oxygen system. However, after one minute, the
%% oxygen spreads to all open (.) locations that are adjacent to a
%% location containing oxygen:

%%  ##
%% #..##
%% #.#..#
%% #OOO#
%%  ###

%% After a total of two minutes, the map looks like this:

%%  ##
%% #..##
%% #O#O.#
%% #OOO#
%%  ###

%% After a total of three minutes:

%%  ##
%% #O.##
%% #O#OO#
%% #OOO#
%%  ###

%% And finally, the whole region is full of oxygen after a total of
%% four minutes:

%%  ##
%% #OO##
%% #O#OO#
%% #OOO#
%%  ###

%% So, in this example, all locations contain oxygen after 4 minutes.

%% Use the repair droid to get a complete map of the area. How many
%% minutes will it take to fill with oxygen?

-mode('compile').

-export([main/1]).

-define(MOVE_NORTH, 1).
-define(MOVE_SOUTH, 2).
-define(MOVE_WEST, 3).
-define(MOVE_EAST, 4).

-define(STATUS_WALL, 0).
-define(STATUS_MOVED, 1).
-define(STATUS_OXYGEN, 2).

-define(RIGHT_BOTTOM, {75, 50}).
%% -define(RIGHT_BOTTOM, {6, 5}).
-define(DROID_START, {25, 25}).

main(_) ->
    Intcode = read_intcode(),
    {Map, Oxygen} = find_oxygen(Intcode),

    %% {Map, {X,Y}} = test_map(),
    %% run it again to find more untravelled spaces
    {MoreMap, Oxygen} = find_oxygen(Intcode, Map, ?DROID_START),
    io:format("found oxygen at ~p~n", [Oxygen]),
    fill_time(MoreMap, [Oxygen], 0).

fill_time(Map, OxygenPoints, Minutes) ->
    case lists:foldl(fun next_fill_points/2, {[], Map}, OxygenPoints) of
        {[], _M} -> io:format("it took ~p minutes to fill~n", [Minutes]);
        {OPs, M} -> fill_time(M, lists:usort(OPs), Minutes+1)
    end.

next_fill_points(OxygenPoint, {OPs, Map}) ->
    Possible = possible(Map, OxygenPoint),
    Points = [Point || {_, Point, {<<".">>, _}} <- Possible],
    {OPs ++ Points
    ,Map#{OxygenPoint => <<"O">>}
    }.

find_oxygen(Intcode) ->
    find_oxygen(Intcode, #{}, ?DROID_START).

find_oxygen(Intcode, Map, DroidXY) ->
    Parent = self(),
    DroidPid = spawn(fun() -> start_droid(Intcode, Parent) end),
    navigate(Map, DroidXY, DroidPid, 1).

navigate(Map, DroidXY, DroidPid, Steps) ->
    receive
        {DroidPid, 'input'} ->
            Direction = select_direction(Map, DroidXY),
            DroidPid ! {self(), Direction},
            navigate(Map, DroidXY, DroidPid, Steps, Direction)
    end.

%% we know that droid starting at {25,25}, Oxygen is at {11,37}
%% so don't let the droid go above y=20,

select_direction(Map, DroidXY) ->
    Possible = possible(Map, DroidXY),
    %% prefer going to new places over encountered
    case lists:keyfind({<<" ">>, 0}, 3, Possible) of
        {Direction, _, _} -> Direction;
        'false' -> direction_less_traveled(Possible)
    end.

possible(Map, DroidXY) ->
    [{Direction, NewXY, maps:get(NewXY, Map, {<<" ">>, 0})}
     || Direction <- [?MOVE_NORTH, ?MOVE_EAST, ?MOVE_SOUTH, ?MOVE_WEST],
        NewXY <- [move_droid(DroidXY, Direction)],
        not_a_wall(Map, NewXY)
    ].

direction_less_traveled([{Dir, _, {_, Cnt}} | Possible]) ->
    {_Count, Directions} =
        lists:foldl(fun({D, _, {_, C}}, {C, Ds}) -> {C, [D | Ds]};
                       ({D, _, {_, C}}, {Count, _Ds}) when C < Count -> {C, [D]};
                       (_, Acc) -> Acc
                    end
                   ,{Cnt, [Dir]}
                   ,Possible
                   ),
    hd(aoc:shuffle_list(Directions)).

not_a_wall(Map, XY) ->
    <<"#">> =/= maps:get(XY, Map, {<<" ">>, 0}).

navigate(Map, DroidXY, DroidPid, Steps, Direction) ->
    receive
        {DroidPid, 'output', ?STATUS_OXYGEN} ->
            DroidPid ! 'done',
            update_map(Map, DroidXY, Direction, ?STATUS_MOVED);
        {DroidPid, 'output', Result} ->
            {UpdatedMap, UpdatedDroidXY} = update_map(Map, DroidXY, Direction, Result),
            navigate(UpdatedMap, UpdatedDroidXY, DroidPid, Steps+1)
    end.

update_map(Map, DroidXY, Direction, ?STATUS_WALL) ->
    NewDroidXY = move_droid(DroidXY, Direction),
    {Map#{NewDroidXY => <<"#">>}, DroidXY};
update_map(Map, DroidXY, Direction, ?STATUS_MOVED) ->
    NewDroidXY = move_droid(DroidXY, Direction),

    {_, LeftCount} = maps:get(DroidXY, Map, {<<".">>, 0}),
    {_, NewCount} = maps:get(NewDroidXY, Map, {<<".">>, 0}),

    {Map#{NewDroidXY => {<<"D">>, NewCount}
         ,DroidXY => {<<".">>, LeftCount+1}
         }
    ,NewDroidXY
    }.

move_droid({X, Y}, ?MOVE_NORTH) ->
    {X, Y-1};
move_droid({X, Y}, ?MOVE_EAST) ->
    {X+1, Y};
move_droid({X, Y}, ?MOVE_SOUTH) ->
    {X, Y+1};
move_droid({X, Y}, ?MOVE_WEST) ->
    {X-1, Y}.

start_droid(Intcode, Parent) ->
    WithInput = intcode:set_input_fun(Intcode, input_fun(Parent)),
    WithOutput = intcode:set_output_fun(WithInput, output_fun(Parent)),
    _V = intcode:run(WithOutput).

output_fun(Parent) ->
    fun(V, _) ->
            Parent ! {self(), 'output', V}
    end.

input_fun(Parent) ->
    fun() ->
            Parent ! {self(), 'input'},
            receive 'done' -> 'done'; {Parent, Input} -> Input end
    end.

read_intcode() ->
    Contents = read_input(),
    intcode:from_binary(Contents).

read_input() ->
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.

%% test_map() ->
%%     binary_to_map(test_input(), #{}, {0, 0}, {0,0}).

%% binary_to_map(<<>>, Map, _, O) -> {Map, O};
%% binary_to_map(<<$\n, Bin/binary>>, Map, {_X, Y}, O) ->
%%     binary_to_map(Bin, Map, {0, Y+1}, O);
%% binary_to_map(<<$O, Bin/binary>>, Map, {X,Y}, _) ->
%%     binary_to_map(Bin, Map#{{X, Y} => <<"O">>}, {X+1, Y}, {X, Y});
%% binary_to_map(<<C:1/binary, Bin/binary>>, Map, {X, Y}, O) ->
%%     binary_to_map(Bin, Map#{{X, Y} => {C, 0}}, {X+1, Y}, O).

%% test_input() ->
%%     <<" ##   \n"
%%       "#..## \n"
%%       "#.#..#\n"
%%       "#.O.#\n"
%%       " ###  \n"
%%     >>.
