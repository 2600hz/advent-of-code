-module(day06).

-export([run/0]).

%% https://adventofcode.com/2023/day/6
%% record breakers: 160816

run() ->
    Input = input("day06.txt"),
    Races = parse_races(Input),
    part1(Races),
    Race = parse_kerned_race(Input),
    part2(Race).

part1(Races) ->
    RecordTimes = record_breakers(Races),
    io:format("record breakers: ~p~n", [lists:foldl(fun(T, P) -> T*P end, 1, RecordTimes)]).

record_breakers(Races) ->
    record_breakers(Races, []).

record_breakers([], RecordTimes) ->
    RecordTimes;
record_breakers([{RaceTime, RecordDistance} | Races], RecordTimes) ->
    Breakers = record_breaker(RaceTime, RecordDistance),
    record_breakers(Races, [lists:sum(Breakers) | RecordTimes]).

record_breaker(RaceTime, RecordDistance) ->
    [1 || HoldTime <- lists:seq(1, RaceTime),
          RecordDistance < distance(RaceTime, HoldTime)
    ].

distance(RaceTime, HoldTime) ->
    (RaceTime - HoldTime) * HoldTime.

part2({RaceTime, RecordDistance}) ->
    Breakers = record_breaker(RaceTime, RecordDistance),
    io:format("kerned race ways to win: ~p~n", [lists:sum(Breakers)]).

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

parse_kerned_race(Input) ->
    [<<"Time: ", TimesBin/binary>>
    ,<<"Distance: ", DistancesBin/binary>>
    ] = binary:split(Input, <<$\n>>, ['trim']),

    Time = binary_to_integer(binary:replace(TimesBin, [<<$\n>>, <<" ">>], <<>>, ['global'])),
    Distance = binary_to_integer(binary:replace(DistancesBin, [<<$\n>>, <<" ">>], <<>>, ['global'])),
    {Time, Distance}.
