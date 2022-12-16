-module(day14).

%% --- Day 14: Regolith Reservoir ---

%% The distress signal leads you to a giant waterfall! Actually, hang
%% on - the signal seems like it's coming from the waterfall itself,
%% and that doesn't make any sense. However, you do notice a little
%% path that leads behind the waterfall.

%% Correction: the distress signal leads you behind a giant waterfall!
%% There seems to be a large cave system here, and the signal
%% definitely leads further inside.

%% As you begin to make your way deeper underground, you feel the
%% ground rumble for a moment. Sand begins pouring into the cave! If
%% you don't quickly figure out where the sand is going, you could
%% quickly become trapped!

%% Fortunately, your familiarity with analyzing the path of falling
%% material will come in handy here. You scan a two-dimensional
%% vertical slice of the cave above you (your puzzle input) and
%% discover that it is mostly air with structures made of rock.

%% Your scan traces the path of each solid rock structure and reports
%% the x,y coordinates that form the shape of the path, where x
%% represents distance to the right and y represents distance
%% down. Each path appears as a single line of text in your
%% scan. After the first point of each path, each point indicates the
%% end of a straight horizontal or vertical line to be drawn from the
%% previous point. For example:

%% 498,4 -> 498,6 -> 496,6
%% 503,4 -> 502,4 -> 502,9 -> 494,9

%% This scan means that there are two paths of rock; the first path
%% consists of two straight lines, and the second path consists of
%% three straight lines. (Specifically, the first path consists of a
%% line of rock from 498,4 through 498,6 and another line of rock from
%% 498,6 through 496,6.)

%% The sand is pouring into the cave from point 500,0.

%% Drawing rock as #, air as ., and the source of the sand as +, this
%% becomes:


%%   4     5  5
%%   9     0  0
%%   4     0  3
%% 0 ......+...
%% 1 ..........
%% 2 ..........
%% 3 ..........
%% 4 ....#...##
%% 5 ....#...#.
%% 6 ..###...#.
%% 7 ........#.
%% 8 ........#.
%% 9 #########.

%% Sand is produced one unit at a time, and the next unit of sand is
%% not produced until the previous unit of sand comes to rest. A unit
%% of sand is large enough to fill one tile of air in your scan.

%% A unit of sand always falls down one step if possible. If the tile
%% immediately below is blocked (by rock or sand), the unit of sand
%% attempts to instead move diagonally one step down and to the
%% left. If that tile is blocked, the unit of sand attempts to instead
%% move diagonally one step down and to the right. Sand keeps moving
%% as long as it is able to do so, at each step trying to move down,
%% then down-left, then down-right. If all three possible destinations
%% are blocked, the unit of sand comes to rest and no longer moves, at
%% which point the next unit of sand is created back at the source.

%% So, drawing sand that has come to rest as o, the first unit of sand
%% simply falls straight down and then stops:

%% ......+...
%% ..........
%% ..........
%% ..........
%% ....#...##
%% ....#...#.
%% ..###...#.
%% ........#.
%% ......o.#.
%% #########.

%% The second unit of sand then falls straight down, lands on the
%% first one, and then comes to rest to its left:

%% ......+...
%% ..........
%% ..........
%% ..........
%% ....#...##
%% ....#...#.
%% ..###...#.
%% ........#.
%% .....oo.#.
%% #########.

%% After a total of five units of sand have come to rest, they form
%% this pattern:

%% ......+...
%% ..........
%% ..........
%% ..........
%% ....#...##
%% ....#...#.
%% ..###...#.
%% ......o.#.
%% ....oooo#.
%% #########.

%% After a total of 22 units of sand:

%% ......+...
%% ..........
%% ......o...
%% .....ooo..
%% ....#ooo##
%% ....#ooo#.
%% ..###ooo#.
%% ....oooo#.
%% ...ooooo#.
%% #########.

%% Finally, only two more units of sand can possibly come to rest:

%% ......+...
%% ..........
%% ......o...
%% .....ooo..
%% ....#ooo##
%% ...o#ooo#.
%% ..###ooo#.
%% ....oooo#.
%% .o.ooooo#.
%% #########.

%% Once all 24 units of sand shown above have come to rest, all
%% further sand flows out the bottom, falling into the endless
%% void. Just for fun, the path any new sand takes before falling
%% forever is shown here with ~:

%% .......+...
%% .......~...
%% ......~o...
%% .....~ooo..
%% ....~#ooo##
%% ...~o#ooo#.
%% ..~###ooo#.
%% ..~..oooo#.
%% .~o.ooooo#.
%% ~#########.
%% ~..........
%% ~..........
%% ~..........

%% Using your scan, simulate the falling sand. How many units of sand
%% come to rest before sand starts flowing into the abyss below?

%% --- Part Two ---

%% You realize you misread the scan. There isn't an endless void at
%% the bottom of the scan - there's floor, and you're standing on it!

%% You don't have time to scan the floor, so assume the floor is an
%% infinite horizontal line with a y coordinate equal to two plus the
%% highest y coordinate of any point in your scan.

%% In the example above, the highest y coordinate of any point is 9,
%% and so the floor is at y=11. (This is as if your scan contained one
%% extra rock path like -infinity,11 -> infinity,11.) With the added
%% floor, the example above now looks like this:

%%         ...........+........
%%         ....................
%%         ....................
%%         ....................
%%         .........#...##.....
%%         .........#...#......
%%         .......###...#......
%%         .............#......
%%         .............#......
%%         .....#########......
%%         ....................
%% <-- etc #################### etc -->

%% To find somewhere safe to stand, you'll need to simulate falling
%% sand until a unit of sand comes to rest at 500,0, blocking the
%% source entirely and stopping the flow of sand into the cave. In the
%% example above, the situation finally looks like this after 93 units
%% of sand come to rest:

%% ............o............
%% ...........ooo...........
%% ..........ooooo..........
%% .........ooooooo.........
%% ........oo#ooo##o........
%% .......ooo#ooo#ooo.......
%% ......oo###ooo#oooo......
%% .....oooo.oooo#ooooo.....
%% ....oooooooooo#oooooo....
%% ...ooo#########ooooooo...
%% ..ooooo.......ooooooooo..
%% #########################

%% Using your scan, simulate the falling sand until the source of the
%% sand becomes blocked. How many units of sand come to rest?

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

part1({RockMap, FloorY, SandSource}) ->
    {_RockMap1, Grains} = drop_sand_until_void(RockMap, FloorY, SandSource, 0),
    io:format("~p sand grains came to rest~n", [Grains]).

drop_sand_until_void(RockMap, FloorY, SandSource, Grains) ->
    case drop_sand(RockMap, FloorY, SandSource, SandSource) of
        {'endless_void', _} -> {RockMap, Grains};
        RockMap1 -> drop_sand_until_void(RockMap1, FloorY, SandSource, Grains+1)
    end.

drop_sand(_RockMap, FloorY, _SandSource, {_X, SandY}=SandXY) when SandY >= FloorY ->
    {'endless_void', SandXY}; % no where to go
drop_sand(RockMap, FloorY, SandSource, SandXY) ->
    NextSandXYs = [step_down(SandXY), step_down_left(SandXY), step_down_right(SandXY)],

    drop_sand(RockMap, FloorY, SandSource, SandXY, NextSandXYs).

drop_sand(_RockMap, _FloorY, SandSource, SandSource, []) ->
    %% filled up the room with sand
    'blocked';
drop_sand(RockMap, _FloorY, _SandSource, SandXY, []) ->
    %% comes to rest
    RockMap#{SandXY => 'sand'};
drop_sand(RockMap, FloorY, SandSource, SandXY, [Step | Steps]) ->
    case maps:get(Step, RockMap, 'air') of
        'air' -> drop_sand(RockMap, FloorY, SandSource, Step);
        _ -> drop_sand(RockMap, FloorY, SandSource, SandXY, Steps)
    end.

step_down({X, Y}) -> {X, Y+1}.
step_down_left({X, Y}) -> {X-1, Y+1}.
step_down_right({X, Y}) -> {X+1, Y+1}.

part2({RockMap, FloorY, SandSource}) ->
    {_RockMap1, Grains} = drop_sand_until_source(RockMap, FloorY+2, SandSource, 0),
    io:format("~p sand grains fell until source was blocked~n", [Grains]).

drop_sand_until_source(RockMap, FloorY, SandSource, Grains) ->
    case drop_sand(RockMap, FloorY, SandSource, SandSource) of
        'blocked' ->
            {RockMap#{SandSource => 'sand'}, Grains+1};
        {'endless_void', SandXY} ->
            drop_sand_until_source(RockMap#{SandXY => 'rock'}, FloorY, SandSource, Grains);
        RockMap1 ->
            drop_sand_until_source(RockMap1, FloorY, SandSource, Grains+1)
    end.

input() ->
    RockLines = binary:split(input:input(<<?MODULE_STRING>>), <<$\n>>, ['global', 'trim']),
    RockMap = parse_rocks(RockLines, #{}),
    LowestRock = maps:fold(fun({_, Y}, 'rock', N) when N < Y -> Y;
                              (_, _, N) -> N
                           end
                          ,0
                          ,RockMap
                          ),
    {RockMap, LowestRock, {500, 0}}.

parse_rocks([], RockMap) -> RockMap;
parse_rocks([RockLine | RockLines], RockMap) ->
    parse_rocks(RockLines, parse_rock(RockLine, RockMap)).

parse_rock(RockLine, RockMap) ->
    XYs = [list_to_tuple([binary_to_integer(N, 10) || N <- binary:split(Coor, <<",">>)])
           || Coor <- binary:split(RockLine, <<" -> ">>, ['global', 'trim'])
          ],
    add_rock_map(XYs, RockMap).

add_rock_map([_], RockMap) ->
    RockMap;
add_rock_map([Start, End | XYs], RockMap) ->
    add_rock_map([End | XYs], add_rock_line(Start, End, RockMap)).

add_rock_line({SX, SY}, {SX, EY}, RockMap) ->
    lists:foldl(fun(Y, RM) -> RM#{{SX, Y} => 'rock'} end
               ,RockMap
               ,apply(lists, seq, lists:sort([SY, EY]))
               );
add_rock_line({SX, SY}, {EX, SY}, RockMap) ->
    lists:foldl(fun(X, RM) -> RM#{{X, SY} => 'rock'} end
               ,RockMap
               ,apply(lists, seq, lists:sort([SX, EX]))
               ).
