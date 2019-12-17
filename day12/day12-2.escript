#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% All this drifting around in space makes you wonder about the nature
%% of the universe. Does history really repeat itself? You're curious
%% whether the moons will ever return to a previous state.

%% Determine the number of steps that must occur before all of the
%% moons' positions and velocities exactly match a previous point in
%% time.

%% For example, the first example above takes 2772 steps before they
%% exactly match a previous point in time; it eventually returns to
%% the initial state:

%% After 0 steps:
%% pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
%% pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
%% pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
%% pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>

%% After 2770 steps:
%% pos=<x=  2, y= -1, z=  1>, vel=<x= -3, y=  2, z=  2>
%% pos=<x=  3, y= -7, z= -4>, vel=<x=  2, y= -5, z= -6>
%% pos=<x=  1, y= -7, z=  5>, vel=<x=  0, y= -3, z=  6>
%% pos=<x=  2, y=  2, z=  0>, vel=<x=  1, y=  6, z= -2>

%% After 2771 steps:
%% pos=<x= -1, y=  0, z=  2>, vel=<x= -3, y=  1, z=  1>
%% pos=<x=  2, y=-10, z= -7>, vel=<x= -1, y= -3, z= -3>
%% pos=<x=  4, y= -8, z=  8>, vel=<x=  3, y= -1, z=  3>
%% pos=<x=  3, y=  5, z= -1>, vel=<x=  1, y=  3, z= -1>

%% After 2772 steps:
%% pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
%% pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
%% pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
%% pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>

%% Of course, the universe might last for a very long time before
%% repeating. Here's a copy of the second example from above:

%% <x=-8, y=-10, z=0>
%% <x=5, y=5, z=10>
%% <x=2, y=-7, z=3>
%% <x=9, y=-8, z=-3>

%% This set of initial positions takes 4686774924 steps before it
%% repeats a previous state! Clearly, you might need to find a more
%% efficient way to simulate the universe.

%% How many steps does it take to reach the first state that exactly
%% matches a previous state?

-mode('compile').

-export([main/1]).

-record(moon, {name, x, y, z, dx=0, dy=0, dz=0}).

main(_) ->
    Moons = read_moons(),
    [XSteps, YSteps, ZSteps] = find_cycles(Moons),
    LCM = lcm([XSteps, YSteps, ZSteps]),
    io:format("~p / ~p / ~p: ~p~n", [XSteps, YSteps, ZSteps, LCM]).

lcm([A, B | L]) ->
    lists:foldl(fun(X, Acc) -> lcm(X, Acc) end, lcm(A, B), L).

lcm(A, B) ->
    A * B div gcd(A, B).

%% Euler's Algorithm
gcd(A, 0) -> A;
gcd(A, B) when A < 0 orelse B < 0 -> gcd(abs(A), abs(B));
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, B) -> gcd(B, A - B * (A div B)).

find_cycles(Moons) ->
    find_cycles(Moons, []
               ,[{#moon.x, #moon.dx}
                ,{#moon.y, #moon.dy}
                ,{#moon.z, #moon.dz}
                ]).

find_cycles(_Moons, Cycles, []) ->
    Cycles;
find_cycles(Moons, Cycles, [{A, Da} | Coords]) ->
    Cycle = find_cycle(Moons, A, Da),
    find_cycles(Moons, [Cycle | Cycles], Coords).

find_cycle(Moons, A, Da) ->
    find_cycle(Moons, initial_values(Moons, A, Da), A, Da, 1).

initial_values(Moons, A, Da) ->
    [{element(#moon.name, Moon), element(A, Moon), element(Da, Moon)} || Moon <- Moons].

find_cycle(Moons, InitialValues, A, Da, Steps) ->
    NewMoons = apply_velocity(apply_gravity(Moons, A, Da), A, Da),
    case initial_values(NewMoons, A, Da) of
        InitialValues -> Steps;
        _ -> find_cycle(NewMoons, InitialValues, A, Da, Steps+1)
    end.

apply_velocity(Moons, A, Da) ->
    lists:keysort(#moon.name, [apply_moon_velocity(Moon, A, Da) || Moon <- Moons]).

apply_moon_velocity(Moon, A, Da) ->
    Set = element(A, Moon)+element(Da, Moon),
    setelement(A, Moon, Set).

apply_gravity(Moons, A, Da) ->
    apply_gravity(Moons, A, Da, []).

apply_gravity([Moon], _A, _Da, Gravitated) -> [Moon | Gravitated];
apply_gravity([Moon | Moons], A, Da, Gravitated) ->
    {NewMoon, A, Da, NewMoons} = lists:foldl(fun gravitate/2, {Moon, A, Da, []}, Moons),
    apply_gravity(NewMoons, A, Da, [NewMoon | Gravitated]).

gravitate(AMoon
         ,{TheMoon, A, Da, NewMoons}
         ) ->
    {ANewMoon, TheNewMoon} = gravitate_moons(AMoon, TheMoon, A, Da),
    {TheNewMoon, A, Da, [ANewMoon | NewMoons]}.

gravitate_moons(MoonA
               ,MoonB
               ,A, Da
               ) ->
    update_positions(MoonA, MoonB, [{element(A, MoonA), element(A, MoonB), Da}]).

update_positions(MoonA, MoonB, []) ->
    {MoonA, MoonB};
update_positions(MoonA, MoonB, [{PosA, PosA, _Index} | Positions]) ->
    update_positions(MoonA, MoonB, Positions);
update_positions(MoonA, MoonB, [{PosA, PosB, Index} | Positions]) when PosA > PosB ->
    update_positions(setelement(Index, MoonA, element(Index, MoonA)-1)
                    ,setelement(Index, MoonB, element(Index, MoonB)+1)
                    ,Positions
                    );
update_positions(MoonA, MoonB, [{PosA, PosB, Index} | Positions]) when PosA < PosB ->
    update_positions(setelement(Index, MoonA, element(Index, MoonA)+1)
                    ,setelement(Index, MoonB, element(Index, MoonB)-1)
                    ,Positions
                    ).

read_moons() ->
    Contents = read_input(),
    to_moons(Contents).

to_moons(Contents) ->
    MoonBins = binary:split(Contents, <<"\n">>, ['global']),
    lists:keysort(#moon.name, [to_moon(MoonBin) || MoonBin <- MoonBins, MoonBin =/= <<>>]).

to_moon(MoonBin) ->
    {'match', Positions} = re:run(MoonBin, <<"<x=([-0-9]+), y=([-0-9]+), z=([-0-9]+)>">>, [{capture, all_but_first, binary}]),
    [X, Y, Z] = [binary_to_integer(Pos) || Pos <- Positions],
    #moon{name=list_to_binary(Positions), x=X, y=Y, z=Z}.

read_input() ->
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.

test_input() ->
    %% <<"<x=-1, y=0, z=2>\n"
    %%   "<x=2, y=-10, z=-7>\n"
    %%   "<x=4, y=-8, z=8>\n"
    %%   "<x=3, y=5, z=-1>"
    %% >>. %% total energy after 10 steps: 179, cycle after 2772 steps
    <<"<x=-8, y=-10, z=0>\n"
      "<x=5, y=5, z=10>\n"
      "<x=2, y=-7, z=3>\n"
      "<x=9, y=-8, z=-3>"
    >>. %% total energy after 100 steps: 1940, cycle after 4686774924 steps
