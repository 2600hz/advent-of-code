-module(day06).

-export([run/0]).

%% https://adventofcode.com/2023/day/6
%% record breakers: 160816

run() ->
    Races = parse_races(input("day06.txt")),
    part1(Races),
    part2(Races).

part1(Races) ->
    RecordTimes = record_breakers(Races),
    io:format("record breakers: ~p~n", [lists:foldl(fun(T, P) -> T*P end, 1, RecordTimes)]).

record_breakers(Races) ->
    record_breakers(Races, []).

record_breakers([], RecordTimes) ->
    RecordTimes;
record_breakers([{RaceTime, RecordDistance} | Races], RecordTimes) ->
    Breakers = [1 || HoldTime <- lists:seq(1, RaceTime),
                     RecordDistance < distance(RaceTime, HoldTime)
               ],
    record_breakers(Races, [lists:sum(Breakers) | RecordTimes]).

distance(RaceTime, HoldTime) ->
    (RaceTime - HoldTime) * HoldTime.

part2(Races) ->
    Races.

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
    io:format("ds: ~s~n", [DistancesBin]),
    {'match', Distances} = re:run(DistancesBin, <<"(\\d+)">>, [{capture, first, binary}, global]),
    lists:zip(Times, lists:map(fun erlang:binary_to_integer/1, [hd(D) || D <- Distances])).
