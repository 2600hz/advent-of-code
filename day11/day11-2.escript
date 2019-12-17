#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% You're not sure what it's trying to paint, but it's definitely not
%% a registration identifier. The Space Police are getting impatient.

%% Checking your external ship cameras again, you notice a white panel
%% marked "emergency hull painting robot starting panel". The rest of
%% the panels are still black, but it looks like the robot was
%% expecting to start on a white panel, not a black one.

%% Based on the Space Law Space Brochure that the Space Police
%% attached to one of your windows, a valid registration identifier is
%% always eight capital letters. After starting the robot on a single
%% white panel instead, what registration identifier does it paint on
%% your hull?

-mode('compile').

-export([main/1]).

%% API
-define(BLACK, 0).
-define(WHITE, 1).

-define(TURN_LEFT, 0).
-define(TURN_RIGHT, 1).

main(_) ->
    Intcode = read_intcode(),
    Panels = paint_panels(Intcode),
    io:format("painted ~p panels~n", [maps:size(Panels)]),
    show_panels(Panels).

show_panels(PaintedPanels) ->
    Panels = [{X, Y, Color} || {{X, Y}, {Color, _}} <- maps:to_list(PaintedPanels)],
    [{Left, _, _}|_] = ByX = lists:keysort(1, Panels),
    [{Right, _, _}|_] = lists:reverse(ByX),

    [{_, Top, _}|_] = ByY = lists:keysort(2, Panels),
    [{_, Bot, _}|_] = lists:reverse(ByY),

    show_panels(PaintedPanels, {Left, Right, Top, Bot}, {Left, Top}).

show_panels(PaintedPanels, {_, Right, _, Bot}, {Right, Bot}=Point) ->
    show_panel(PaintedPanels, Point),
    io:format("~n");
show_panels(PaintedPanels, {Left, Right, Top, Bot}, Point) ->
    show_panel(PaintedPanels, Point),
    show_panels(PaintedPanels, {Left, Right, Top, Bot}
               ,next_point({Left, Right, Top, Bot}, Point)
               ).

show_panel(Panels, Point) ->
    {Color, _} = maps:get(Point, Panels, {?BLACK, 0}),
    io:format("~s", [color_to_char(Color)]).

color_to_char(?BLACK) -> " ";
color_to_char(?WHITE) -> "O".

next_point({Left, Right, _Top, _Bot}, {Right, Y}) ->
    io:format("~n"),
    {Left, Y+1};
next_point(_, {X, Y}) -> {X+1, Y}.

paint_panels(Intcode) ->
    Parent = self(),
    IntcodePid = spawn(fun() -> run_intcode(Intcode, Parent) end),

    paint_panels(IntcodePid, #{{0,0} => {?WHITE, 0}}, {{0,0},'up'}, 'paint').

%% #{{X,Y} => {black/white, painted} }
paint_panels(IntcodePid, Panels, RobotPosition, RobotAction) ->
    receive
        {IntcodePid, 'input'} ->
            {PaintColor, _Painted} = read_camera(Panels, RobotPosition),
            IntcodePid ! {self(), 'input', PaintColor},
            paint_panels(IntcodePid, Panels, RobotPosition, RobotAction);
        {IntcodePid, 'output', Instruction} ->
            {UpdatedPanels, UpdatedRobot, UpdatedAction} = run_action(Panels, RobotPosition, RobotAction, Instruction),
            paint_panels(IntcodePid, UpdatedPanels, UpdatedRobot, UpdatedAction);
        {IntcodePid, 'done'} -> Panels
    end.

read_camera(Panels, {Point, _Heading}) ->
    maps:get(Point, Panels, {?BLACK, 0}).

run_action(Panels, {Point, _Heading}=RobotPosition, 'paint', Color) ->
    {_, Painted} = read_camera(Panels, RobotPosition),
    {Panels#{Point => {Color, Painted+1}}, RobotPosition, 'move'};
run_action(Panels, RobotPosition, 'move', Turn) ->
    {Panels, turn_robot(RobotPosition, Turn), 'paint'}.

turn_robot({{X, Y}, 'up'}, ?TURN_LEFT) ->
    {{X-1, Y}, 'left'};
turn_robot({{X, Y}, 'left'}, ?TURN_LEFT) ->
    {{X, Y+1}, 'down'};
turn_robot({{X, Y}, 'down'}, ?TURN_LEFT) ->
    {{X+1, Y}, 'right'};
turn_robot({{X, Y}, 'right'}, ?TURN_LEFT) ->
    {{X, Y-1}, 'up'};

turn_robot({{X, Y}, 'up'}, ?TURN_RIGHT) ->
    {{X+1, Y}, 'right'};
turn_robot({{X, Y}, 'right'}, ?TURN_RIGHT) ->
    {{X, Y+1}, 'down'};
turn_robot({{X, Y}, 'down'}, ?TURN_RIGHT) ->
    {{X-1, Y}, 'left'};
turn_robot({{X, Y}, 'left'}, ?TURN_RIGHT) ->
    {{X, Y-1}, 'up'}.

run_intcode(Intcode, Parent) ->
    WithOutput = intcode:set_output_fun(Intcode
                                       ,fun(N, _) -> Parent ! {self(), 'output', N} end
                                       ),
    WithInput = intcode:set_input_fun(WithOutput
                                     ,fun() ->
                                              Parent ! {self(), 'input'},
                                              receive
                                                  {Parent, 'input', N} -> N
                                              end
                                      end
                                     ),
    _V = intcode:run(WithInput),
    Parent ! {self(), 'done'}.

read_intcode() ->
    Contents = read_input(),
    intcode:from_binary(Contents).

read_input() ->
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.
