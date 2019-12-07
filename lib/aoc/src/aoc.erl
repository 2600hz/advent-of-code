-module(aoc).

%% Day 1 helpers
-export([mass_fuel_req/1
        ,read_masses/1
        ]).

%% Day 3 helpers
-export([manhattan_distance/2]).

-export([permutations/1]).

mass_fuel_req(Mass) ->
    (Mass div 3) - 2.

read_masses(InputFile) ->
    {'ok', IODevice} = file:open(InputFile, ['read']),
    read_masses(IODevice, file:read_line(IODevice), []).

read_masses(IODevice, 'eof', Masses) ->
    file:close(IODevice),
    Masses;
read_masses(IODevice, {'ok', Line}, Masses) ->
    read_masses(IODevice, file:read_line(IODevice), [list_to_integer(Line -- [$\n]) | Masses]).

manhattan_distance({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

%% from http://erlang.org/doc/programming_examples/list_comprehensions.html
permutations([]) -> [[]];
permutations(L) ->
    [[H|T] || H <- L, T <- permutations(L--[H])].
