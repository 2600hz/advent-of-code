-module(day1).
-export([calc_fuel_for_modules/0, calc_total_fuel/0]).

read_input(FileName) ->
    {ok, Data} = file:read_file(FileName),
    lists:delete(<<>>, binary:split(Data, [<<"\n">>], [global])).

process_input(L) ->
    [ list_to_integer(binary_to_list(X)) || X <- L ].

calc_module_fuel(X) ->
    X div 3 - 2.

calc_fuel_for_fuel(X) ->
    calc_fuel_for_fuel(X, 0).

calc_fuel_for_fuel(X, Total) ->
    Fuel = X div 3 - 2,
    case Fuel >= 0 of
        true -> calc_fuel_for_fuel(Fuel, Total + Fuel);
        false -> Total
    end.

calc_total_fuel_for_module(X) ->
    ModuleFuel = X div 3 - 2,
    FuelForFuel = calc_fuel_for_fuel(ModuleFuel),
    ModuleFuel + FuelForFuel.

calc_fuel_for_modules() ->
    Input = process_input(read_input("day1.input")),
    lists:sum([ calc_module_fuel(X) || X <- Input]).

calc_total_fuel() ->
    Input = process_input(read_input("day1.input")),
    lists:sum([ calc_total_fuel_for_module(X) || X <- Input]).
