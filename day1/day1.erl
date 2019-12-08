-module(day1).
-export([test/0]).

read_input(InputFile) ->
    {'ok', IODevice} = file:open(InputFile, ['read']),
    read_input(IODevice, file:read_line(IODevice), []).

read_input(IODevice, 'eof', Lines) ->
    file:close(IODevice),
    Lines;
read_input(IODevice, {'ok', Line}, Lines) ->
    read_input(IODevice, file:read_line(IODevice), [list_to_integer(Line -- "\n") | Lines]).

mass_fuel_req(Mass) ->
    (Mass div 3) - 2.

mass_and_fuel_req(Mass) ->
    mass_and_fuel_req(mass_fuel_req(Mass), 0).

mass_and_fuel_req(Mass, Total) when Mass =< 0 ->
    Total;
mass_and_fuel_req(Mass, Total) ->
    mass_and_fuel_req(mass_fuel_req(Mass), Mass + Total).

test() ->
    lists:sum([ mass_and_fuel_req(Fuel) || Fuel <- read_input("day1.input")]).
