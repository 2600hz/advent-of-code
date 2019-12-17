#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Day 11: Space Police ---

%% On the way to Jupiter, you're pulled over by the Space Police.

%% "Attention, unmarked spacecraft! You are in violation of Space Law!
%% All spacecraft must have a clearly visible registration identifier!
%% You have 24 hours to comply or be sent to Space Jail!"

%% Not wanting to be sent to Space Jail, you radio back to the Elves
%% on Earth for help. Although it takes almost three hours for their
%% reply signal to reach you, they send instructions for how to power
%% up the emergency hull painting robot and even provide a small
%% Intcode program (your puzzle input) that will cause it to paint
%% your ship appropriately.

%% There's just one problem: you don't have an emergency hull painting
%% robot.

%% You'll need to build a new emergency hull painting robot. The robot
%% needs to be able to move around on the grid of square panels on the
%% side of your ship, detect the color of its current panel, and paint
%% its current panel black or white. (All of the panels are currently
%% black.)

%% The Intcode program will serve as the brain of the robot. The
%% program uses input instructions to access the robot's camera:
%% provide 0 if the robot is over a black panel or 1 if the robot is
%% over a white panel. Then, the program will output two values:

%%     First, it will output a value indicating the color to paint the
%%     panel the robot is over: 0 means to paint the panel black, and
%%     1 means to paint the panel white.
%%     Second, it will output a value indicating the direction the
%%     robot should turn: 0 means it should turn left 90 degrees, and
%%     1 means it should turn right 90 degrees.

%% After the robot turns, it should always move forward exactly one
%% panel. The robot starts facing up.

%% The robot will continue running for a while like this and halt when
%% it is finished drawing. Do not restart the Intcode computer inside
%% the robot during this process.

%% For example, suppose the robot is about to start running. Drawing
%% black panels as ., white panels as #, and the robot pointing the
%% direction it is facing (< ^ > v), the initial state and region near
%% the robot looks like this:

%% .....
%% .....
%% ..^..
%% .....
%% .....

%% The panel under the robot (not visible here because a ^ is shown
%% instead) is also black, and so any input instructions at this point
%% should be provided 0. Suppose the robot eventually outputs 1 (paint
%% white) and then 0 (turn left). After taking these actions and
%% moving forward one panel, the region now looks like this:

%% .....
%% .....
%% .<#..
%% .....
%% .....

%% Input instructions should still be provided 0. Next, the robot
%% might output 0 (paint black) and then 0 (turn left):

%% .....
%% .....
%% ..#..
%% .v...
%% .....

%% After more outputs (1,0, 1,0):

%% .....
%% .....
%% ..^..
%% .##..
%% .....

%% The robot is now back where it started, but because it is now on a
%% white panel, input instructions should be provided 1. After several
%% more outputs (0,1, 1,0, 1,0), the area looks like this:

%% .....
%% ..<#.
%% ...#.
%% .##..
%% .....

%% Before you deploy the robot, you should probably have an estimate
%% of the area it will cover: specifically, you need to know the
%% number of panels it paints at least once, regardless of color. In
%% the example above, the robot painted 6 panels at least once. (It
%% painted its starting panel twice, but that panel is still only
%% counted once; it also never painted the panel it ended on.)

%% Build a new emergency hull painting robot and run the Intcode
%% program on it. How many panels does it paint at least once?

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
    io:format("painted ~p panels~n", [maps:size(Panels)]).

paint_panels(Intcode) ->
    Parent = self(),
    IntcodePid = spawn(fun() -> run_intcode(Intcode, Parent) end),

    paint_panels(IntcodePid, #{}, {0,0,'up'}, 'paint').

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

read_camera(Panels, {X, Y, _Heading}) ->
    maps:get({X, Y}, Panels, {?BLACK, 0}).

run_action(Panels, {X, Y, _Heading}=RobotPosition, 'paint', Color) ->
    {_, Painted} = read_camera(Panels, RobotPosition),
    {Panels#{{X, Y} => {Color, Painted+1}}, RobotPosition, 'move'};
run_action(Panels, RobotPosition, 'move', Turn) ->
    {Panels, turn_robot(RobotPosition, Turn), 'paint'}.

turn_robot({X, Y, 'up'}, ?TURN_LEFT) ->
    {X-1, Y, 'left'};
turn_robot({X, Y, 'left'}, ?TURN_LEFT) ->
    {X, Y+1, 'down'};
turn_robot({X, Y, 'down'}, ?TURN_LEFT) ->
    {X+1, Y, 'right'};
turn_robot({X, Y, 'right'}, ?TURN_LEFT) ->
    {X, Y-1, 'up'};

turn_robot({X, Y, 'up'}, ?TURN_RIGHT) ->
    {X+1, Y, 'right'};
turn_robot({X, Y, 'right'}, ?TURN_RIGHT) ->
    {X, Y+1, 'down'};
turn_robot({X, Y, 'down'}, ?TURN_RIGHT) ->
    {X-1, Y, 'left'};
turn_robot({X, Y, 'left'}, ?TURN_RIGHT) ->
    {X, Y-1, 'up'}.

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
