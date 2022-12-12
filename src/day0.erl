-module(day0).

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

part1(Input) ->
    io:format("input: ~p~n", [Input]).

part2(_Input) ->
    'ok'.

input() ->
    input:sample(<<?MODULE_STRING>>).
