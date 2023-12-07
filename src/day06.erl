-module(day06).

-export([run/0]).

%% https://adventofcode.com/2023/day/6
%% record breakers: 160816
%% kerned race ways to win: 46561107

run() ->
    Input = input("day06.txt"),
    Races = parse_races(Input),
    part1(Races),
    Race = parse_kerned_race(Input),
    part2(Race).

part1(Races) ->
    RecordTimes = record_breakers(Races),
    io:format("record breakers: ~p~n", [RecordTimes]).

record_breakers(Races) ->
    record_breakers(Races, 1).

record_breakers([], RecordTimes) ->
    RecordTimes;
record_breakers([{RaceTime, RecordDistance} | Races], RecordTimes) ->
    Breakers = count_winning_times(RaceTime, RecordDistance),
    record_breakers(Races, Breakers*RecordTimes).

%% see Reduced quadratic equation in https://en.wikipedia.org/wiki/Quadratic_equation
%% ax^2 + bx + c = hold^2 - racetime * hold + distance
%% a = 1 b=-racetime c=distance
%% (-b +/- sqrt(b^2 - 4ac)) / 2a
%% (Racetime +/- sqrt(Racetime^2 - 4 * 1 * Distance)) / 2
count_winning_times(RaceTime, RecordDistance) ->
    Root = math:sqrt(RaceTime*RaceTime - (4  * RecordDistance)),
    Upper = math:floor((RaceTime + Root) / 2),
    Lower = math:ceil((RaceTime - Root) / 2),
    round(Upper - Lower + 1).

part2({RaceTime, RecordDistance}) ->
    Breakers = count_winning_times(RaceTime, RecordDistance),
    io:format("kerned race ways to win: ~p~n", [Breakers]).

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.

parse_races(Input) ->
    [<<"Time: ", TimesBin/binary>>
    ,<<"Distance: ", DistancesBin/binary>>
    ] = binary:split(Input, <<$\n>>, ['trim']),

    Times = [binary_to_integer(TimeBin)
             || TimeBin <- binary:split(TimesBin, <<" ">>, ['global', 'trim']),
                <<>> =/= TimeBin
            ],
    {'match', Distances} = re:run(DistancesBin, <<"(\\d+)">>, [{capture, first, binary}, global]),
    lists:zip(Times, lists:map(fun erlang:binary_to_integer/1, [hd(D) || D <- Distances])).

parse_kerned_race(Input) ->
    [<<"Time: ", TimesBin/binary>>
    ,<<"Distance: ", DistancesBin/binary>>
    ] = binary:split(Input, <<$\n>>, ['trim']),

    Time = binary_to_integer(binary:replace(TimesBin, [<<$\n>>, <<" ">>], <<>>, ['global'])),
    Distance = binary_to_integer(binary:replace(DistancesBin, [<<$\n>>, <<" ">>], <<>>, ['global'])),
    {Time, Distance}.
