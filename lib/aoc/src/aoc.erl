-module(aoc).

%% Day 1 helpers
-export([mass_fuel_req/1
        ,read_masses/1
        ]).

%% Day 3 helpers
-export([manhattan_distance/2]).

-export([permutations/1
        ,shuffle_list/1
        ]).

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

-spec shuffle_list(list()) -> list().
shuffle_list([]) -> [];
shuffle_list(List) when is_list(List) ->
    randomize_list(round(math:log(length(List)) + 0.5), List).

-spec randomize_list(list()) -> list().
randomize_list(List) ->
    D = lists:keysort(1, [{rand:uniform(), A} || A <- List]),
    {_, D1} = lists:unzip(D),
    D1.

-spec randomize_list(pos_integer(), list()) -> list().
randomize_list(1, List) -> randomize_list(List);
randomize_list(T, List) ->
    lists:foldl(fun(_E, Acc) ->
                        randomize_list(Acc)
                end
               ,randomize_list(List)
               ,lists:seq(1, (T - 1))
               ).
