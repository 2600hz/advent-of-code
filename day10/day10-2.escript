#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% Once you give them the coordinates, the Elves quickly deploy an
%% Instant Monitoring Station to the location and discover the worst:
%% there are simply too many asteroids.

%% The only solution is complete vaporization by giant laser.

%% Fortunately, in addition to an asteroid scanner, the new monitoring
%% station also comes equipped with a giant rotating laser perfect for
%% vaporizing asteroids. The laser starts by pointing up and always
%% rotates clockwise, vaporizing any asteroid it hits.

%% If multiple asteroids are exactly in line with the station, the
%% laser only has enough power to vaporize one of them before
%% continuing its rotation. In other words, the same asteroids that
%% can be detected can be vaporized, but if vaporizing one asteroid
%% makes another one detectable, the newly-detected asteroid won't be
%% vaporized until the laser has returned to the same position by
%% rotating a full 360 degrees.

%% For example, consider the following map, where the asteroid with
%% the new monitoring station (and laser) is marked X:

%% .#....#####...#..
%% ##...##.#####..##
%% ##...#...#.#####.
%% ..#.....X...###..
%% ..#.#.....#....##

%% The first nine asteroids to get vaporized, in order, would be:

%% .#....###24...#..
%% ##...##.13#67..9#
%% ##...#...5.8####.
%% ..#.....X...###..
%% ..#.#.....#....##

%% Note that some asteroids (the ones behind the asteroids marked 1,
%% 5, and 7) won't have a chance to be vaporized until the next full
%% rotation. The laser continues rotating; the next nine to be
%% vaporized are:

%% .#....###.....#..
%% ##...##...#.....#
%% ##...#......1234.
%% ..#.....X...5##..
%% ..#.9.....8....76

%% The next nine to be vaporized are then:

%% .8....###.....#..
%% 56...9#...#.....#
%% 34...7...........
%% ..2.....X....##..
%% ..1..............

%% Finally, the laser completes its first full rotation (1 through 3),
%% a second rotation (4 through 8), and vaporizes the last asteroid
%% (9) partway through its third rotation:

%% ......234.....6..
%% ......1...5.....7
%% .................
%% ........X....89..
%% .................

%% In the large example above (the one with the best monitoring
%% station location at 11,13):

%%     The 1st asteroid to be vaporized is at 11,12.
%%     The 2nd asteroid to be vaporized is at 12,1.
%%     The 3rd asteroid to be vaporized is at 12,2.
%%     The 10th asteroid to be vaporized is at 12,8.
%%     The 20th asteroid to be vaporized is at 16,0.
%%     The 50th asteroid to be vaporized is at 16,9.
%%     The 100th asteroid to be vaporized is at 10,16.
%%     The 199th asteroid to be vaporized is at 9,6.
%%     The 200th asteroid to be vaporized is at 8,2.
%%     The 201st asteroid to be vaporized is at 10,9.
%%     The 299th and final asteroid to be vaporized is at 11,1.

%% The Elves are placing bets on which will be the 200th asteroid to
%% be vaporized. Win the bet by determining which asteroid that will
%% be; what do you get if you multiply its X coordinate by 100 and
%% then add its Y coordinate? (For example, 8,2 becomes 802.)

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    Asteroids = read_asteroids(),
    Vectorized = vectorize(Asteroids),
    Counts = count_seen(Vectorized),
    [{Best, _} | _] = lists:reverse(lists:keysort(2, Counts)),
    RelativeToBest = vectors_relative_to_best(Best, Vectorized),
    vaporize(200, RelativeToBest).

vectors_relative_to_best(Best, Vectorized) ->
    {Best, Relative} = lists:foldl(fun relative_fold/2, {Best, []}, Vectorized),
    Relative.

relative_fold({Best, B, Angle, Mag}, {Best, Relative}) ->
    {Best, [{B, Angle, Mag} | Relative]};
relative_fold({A, Best, _Angle, _Mag}, {Best, Relative}) ->
    {Best, [{A, angle(Best, A), magnitude(Best, A)} | Relative]};
relative_fold(_, {Best, Relative}) -> {Best, Relative}.

vaporize(Nth, Vectorized) ->
    ByAngleThenMag = sort_by_magnitude(Vectorized),

    Pi = math:pi(),
    StartAt = Pi / -2, % straight up is -pi/2

    find_nth(Nth, 1, find_start(StartAt, queue:from_list(ByAngleThenMag))).

find_nth(Nth, Nth, ByAngleThenMag) ->
    {{'value', {_Angle, [{{X, Y}=Asteroid, _Mag}|_]}}, _Queue} = queue:out(ByAngleThenMag),
    io:format("~pth asteroid vaporized: ~p~n", [Nth, Asteroid]),
    io:format("~p * 100 + ~p = ~p~n", [X, Y, (X*100)+Y]);
find_nth(Nth, N, ByAngleThenMag) ->
    case queue:out(ByAngleThenMag) of
        {{'value', {_Angle, [_Mag]}}, Queue} ->
            %% io:format(" vaporize ~p: ~p~n", [N, _Mag]),
            find_nth(Nth, N+1, Queue);
        {{'value', {Angle, [_Mag | Mags]}}, Queue} ->
            %% io:format(" vaporize ~p: ~p~n", [N, _Mag]),
            find_nth(Nth, N+1, queue:in({Angle, Mags}, Queue))
    end.

find_start(StartAt, ByAngleThenMag) ->
    case queue:out(ByAngleThenMag) of
        {{'value', {Angle, Mags}}, _Queue} when Angle >= StartAt ->
            io:format("found first angle ~p: ~p~n", [Angle, Mags]),
            ByAngleThenMag;
        {{'value', AngleThenMags}, Queue} ->
            find_start(StartAt, queue:in(AngleThenMags, Queue))
    end.

sort_by_magnitude([{A, Angle, Mag} | Asteroids]) ->
    ByAngle = lists:foldl(fun sort_by_mag_fold/2, #{Angle => [{A, Mag}]}, Asteroids),
    lists:keysort(1, [{An, lists:keysort(2, Mags)} || {An, Mags} <- maps:to_list(ByAngle)]).

sort_by_mag_fold({A, Angle, Mag}, Acc) ->
    Mags = maps:get(Angle, Acc, []),
    maps:put(Angle, [{A, Mag} | Mags], Acc).

count_seen(Vectorized) ->
    count_seen(Vectorized, #{}).

count_seen([], Seen) ->
    Counts = maps:fold(fun({A, _}, {B, _}, Acc) ->
                               [{A, 1}, {B, 1} | Acc]
                       end
                      ,[]
                      ,Seen
                      ),
    lists:foldl(fun({A, 1}, [{A, N} | Acc]) ->
                        [{A, N+1} | Acc];
                   ({B, 1}, Acc) ->
                        [{B, 1} | Acc]
                end
               ,[]
               ,lists:sort(Counts)
               );
count_seen([{A, B, Angle, Magnitude} | Vectors], Seen) ->
    case maps:get({A, Angle}, Seen, 'undefined') of
        'undefined' ->
            count_seen(Vectors, maps:put({A, Angle}, {B, Magnitude}, Seen));
        {_B, M} when Magnitude < M ->
            count_seen(Vectors, maps:put({A, Angle}, {B, Magnitude}, Seen));
        _Smaller ->
            count_seen(Vectors, Seen)
    end.

vectorize(Asteroids) ->
    vectorize(Asteroids, []).

vectorize([], Vectors) -> lists:keysort(3, Vectors);
vectorize([Asteroid | Asteroids], Vectors) ->
    NewVectors = lists:foldl(fun(A, Acc) ->
                                     [First, Second] = lists:sort([Asteroid, A]),
                                     [{First, Second, angle(First, Second), magnitude(First, Second)} | Acc]
                             end
                            ,Vectors
                            ,Asteroids
                            ),
    vectorize(Asteroids, NewVectors).

angle({Vx, Vy}, {Wx, Wy}) ->
    X = Wx-Vx,
    Y = Wy-Vy,
    math:atan2(Y, X).

magnitude({X1, Y1}, {X2, Y2}) ->
    Vx = X2-X1,
    Vy = Y2-Y1,
    math:sqrt((Vx*Vx) + (Vy*Vy)).

read_asteroids() ->
    Contents = read_input(),
    from_binary(Contents).

read_input() ->
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.

from_binary(Contents) ->
    {_, Asteroids} = lists:foldl(fun row_to_asteroids/2, {0, []}, binary:split(Contents, <<"\n">>, ['global'])),
    Asteroids.

row_to_asteroids(<<>>, {Y, Acc}) -> {Y, Acc};
row_to_asteroids(Row, {Y, Acc}) ->
    {_X, Y, Acc1} = lists:foldl(fun sky_object_to_asteroid/2, {0, Y, Acc}, binary_to_list(Row)),
    {Y+1, Acc1}.

sky_object_to_asteroid($., {X, Y, Acc}) ->
    {X+1, Y, Acc};
sky_object_to_asteroid($#, {X, Y, Acc}) ->
    {X+1, Y, [{X, Y} | Acc]};
sky_object_to_asteroid($X, {X, Y, Acc}) ->
    {X+1, Y, [{X, Y} | Acc]}.

test_input() ->
    <<".#....#####...#..\n"
      "##...##.#####..##\n"
      "##...#...#.#####.\n"
      "..#.....X...###..\n"
      "..#.#.....#....##\n"
    >>.
%% <<".#.\n"
%%   "###\n"
%%   ".#.\n"
%% >>. %% {1,1} = 2
%% <<".#..#\n"
%%   ".....\n"
%%   "#####\n"
%%   "....#\n"
%%   "...##\n"
%% >>. %% {3,4} = 8
%% <<"......#.#.\n"
%%   "#..#.#....\n"
%%   "..#######.\n"
%%   ".#.#.###..\n"
%%   ".#..#.....\n"
%%   "..#....#.#\n"
%%   "#..#....#.\n"
%%   ".##.#..###\n"
%%   "##...#..#.\n"
%%   ".#....####"
%% >>. %% = {5,8} = 33
