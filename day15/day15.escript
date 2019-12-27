#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Day 15: Oxygen System ---

%% Out here in deep space, many things can go wrong. Fortunately, many
%% of those things have indicator lights. Unfortunately, one of those
%% lights is lit: the oxygen system for part of the ship has failed!

%% According to the readouts, the oxygen system must have failed days
%% ago after a rupture in oxygen tank two; that section of the ship
%% was automatically sealed once oxygen levels went dangerously low. A
%% single remotely-operated repair droid is your only option for
%% fixing the oxygen system.

%% The Elves' care package included an Intcode program (your puzzle
%% input) that you can use to remotely control the repair droid. By
%% running that program, you can direct the repair droid to the oxygen
%% system and fix the problem.

%% The remote control program executes the following steps in a loop
%% forever:

%%     Accept a movement command via an input instruction.
%%     Send the movement command to the repair droid.
%%     Wait for the repair droid to finish the movement operation.
%%     Report on the status of the repair droid via an output instruction.

%% Only four movement commands are understood: north (1), south (2),
%% west (3), and east (4). Any other command is invalid. The movements
%% differ in direction, but not in distance: in a long enough
%% east-west hallway, a series of commands like 4,4,4,4,3,3,3,3 would
%% leave the repair droid back where it started.

%% The repair droid can reply with any of the following status codes:

%%     0: The repair droid hit a wall. Its position has not changed.
%%     1: The repair droid has moved one step in the requested direction.
%%     2: The repair droid has moved one step in the requested
%%     direction; its new position is the location of the oxygen
%%     system.

%% You don't know anything about the area around the repair droid, but
%% you can figure it out by watching the status codes.

%% For example, we can draw the area using D for the droid, # for
%% walls, . for locations the droid can traverse, and empty space for
%% unexplored locations. Then, the initial state looks like this:

%%    D

%% To make the droid go north, send it 1. If it replies with 0, you
%% know that location is a wall and that the droid didn't move:

%%    #
%%    D

%% To move east, send 4; a reply of 1 means the movement was
%% successful:

%%    #
%%    .D

%% Then, perhaps attempts to move north (1), south (2), and east (4)
%% are all met with replies of 0:

%%    ##
%%    .D#
%%     #

%% Now, you know the repair droid is in a dead end. Backtrack with 3
%% (which you already know will get a reply of 1 because you already
%% know that location is open):

%%    ##
%%    D.#
%%     #

%% Then, perhaps west (3) gets a reply of 0, south (2) gets a reply of
%% 1, south again (2) gets a reply of 0, and then west (3) gets a
%% reply of 2:

%%    ##

%%   #..#
%%   D.#
%%    #

%% Now, because of the reply of 2, you know you've found the oxygen
%% system! In this example, it was only 2 moves away from the repair
%% droid's starting position.

%% What is the fewest number of movement commands required to move the
%% repair droid from its starting position to the location of the
%% oxygen system?

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
-define(DROID_START, {25, 25}).

main(_) ->
    Intcode = read_intcode(),
    {Map, {X,Y}} = find_oxygen(Intcode),
    %% run it again to find more untravelled spaces
    {MoreMap, {X, Y}} = find_oxygen(Intcode, Map, ?DROID_START),
    io:format("found oxygen at ~p~n", [{X, Y}]),

    %% Highlights = MoreMap#{?DROID_START => {<<"D">>, 0}
    %%                      ,{X,Y} => {<<"O">>, 0}
    %%                      },
    %% pp_map(Highlights, 0),
    {'true', _Path, _Steps} = shortest_path(MoreMap, {X, Y}, ?DROID_START).

shortest_path(Map, Oxygen, Droid) ->
    shortest_path(Map, Oxygen, Droid, 0).

shortest_path(Map, Oxygen, Oxygen, Steps) ->
    io:format("found oxygen in ~p steps~n", [Steps]),
    %% pp_map(Map#{?DROID_START => {<<"D">>, 0}}, 0),
    {'true', Map, Steps};
shortest_path(Map, Oxygen, Droid, Steps) ->
    case [Direction || {Direction, {Visited, _}} <- possible(Map, Droid), Visited =/= <<"o">>] of
        [] ->
            {'false', Map#{Droid => <<"#">>}};
        [Direction] ->
            shortest_path(Map, Oxygen, Droid, Steps, Direction);
        Directions ->
            shortest_path(Map, Oxygen, Droid, Steps, Directions)
    end.

shortest_path(Map, _Oxygen, Droid, _Steps, []) ->
    {'false', Map#{Droid => <<"#">>}};
shortest_path(Map, Oxygen, Droid, Steps, [Direction | Directions]) ->
    case shortest_path(Map, Oxygen, Droid, Steps, Direction) of
        {'true', _, _}=Found -> Found;
        {'false', _DeadEnd} ->
            shortest_path(Map, Oxygen, Droid, Steps, Directions)
    end;
shortest_path(Map, Oxygen, Droid, Steps, Direction) ->
    Moved = move_droid(Droid, Direction),
    case shortest_path(Map#{Droid => {<<"o">>, 0}}, Oxygen, Moved, Steps+1) of
        {'false', DeadEnd} -> {'false', DeadEnd#{Droid => <<"#">>}};
        {'true', _, _}=Found -> Found
    end.

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
    case lists:keyfind({<<" ">>, 0}, 2, Possible) of
        {Direction, _} -> Direction;
        'false' -> direction_less_traveled(Possible)
    end.

possible(Map, DroidXY) ->
    [{Direction, maps:get(NewXY, Map, {<<" ">>, 0})}
     || Direction <- [?MOVE_NORTH, ?MOVE_EAST, ?MOVE_SOUTH, ?MOVE_WEST],
        NewXY <- [move_droid(DroidXY, Direction)],
        not_a_wall(Map, NewXY)
    ].

direction_less_traveled([{Dir, {_, Cnt}} | Possible]) ->
    {_Count, Directions} =
        lists:foldl(fun({D, {_, C}}, {C, Ds}) -> {C, [D | Ds]};
                       ({D, {_, C}}, {Count, _Ds}) when C < Count -> {C, [D]};
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
            io:format("took ~p steps to find oxygen~n", [Steps]),
            DroidPid ! 'done',
            update_map(Map, DroidXY, Direction, ?STATUS_MOVED);
        {DroidPid, 'output', Result} ->
            {UpdatedMap, UpdatedDroidXY} = update_map(Map, DroidXY, Direction, Result),
            %% pp_map(UpdatedMap, Steps),
            navigate(UpdatedMap, UpdatedDroidXY, DroidPid, Steps+1)
    end.

pp_map(Map, 0) ->
    pp_map(Map, ?RIGHT_BOTTOM, {0, 0});
pp_map(Map, Steps) when Steps rem 20 =:= 0 ->
    io:format("~nstep ~p~n", [Steps]),
    pp_map(Map, ?RIGHT_BOTTOM, {0, 0});
pp_map(_Map, _Steps) -> 'ok'.

pp_map(Map, {Right, Bottom}, {Right, Bottom}) ->
    pp_space(maps:get({Right, Bottom}, Map, {<<" ">>, 0})),
    io:format("~n");
pp_map(Map, {Right, _Bottom}=RB, {Right, Y}) ->
    pp_space(maps:get({Right, Y}, Map, {<<" ">>, 0})),
    io:format("~n"),
    pp_map(Map, RB, {0, Y+1});
pp_map(Map, RB, {X, Y}) ->
    pp_space(maps:get({X, Y}, Map, {<<" ">>, 0})),
    pp_map(Map, RB, {X+1, Y}).

pp_space(<<"#">>) -> io:format("#");
pp_space({Space, _Count}) -> io:format("~s", [Space]).

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
    _V = intcode:run(WithOutput),
    io:format("~p done running: ~p~n", [self(), _V]).

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
