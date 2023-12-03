-module(day3).

-export([sum_part_numbers/1
        ,sum_gear_ratios/1
        ]).

sum_part_numbers(Input) ->
    {Numbers, Symbols} = parse_schematic(Input),
    PartNumbers = lists:filter(part_number_filter(Symbols), Numbers),
    lists:sum([Number || {_, _, _, Number} <- PartNumbers]).

sum_gear_ratios(Input) ->
    {Numbers, Symbols} = parse_schematic(Input),
    PossibleGears = lists:filter(fun is_possible_gear/1, Symbols),
    GearRatios = lists:foldl(gear_ratio_folder(Numbers), [], PossibleGears),
    lists:sum([GearRatio || {_, _, _, GearRatio} <- GearRatios]).

part_number_filter(Symbols) ->
    fun({X, Y, Length, _Number}) ->
        lists:any(fun({SX, SY, _Symbol}) ->
                          SX >= X - 1 andalso SX =< X + Length
                              andalso SY >= Y - 1 andalso SY =< Y + 1
                  end, Symbols)
    end.

gear_ratio_folder(Numbers) ->
    fun({SX, SY, Symbol}, GearRatios) ->
        AdjacentNumbers =
          lists:filter(fun({X, Y, Length, _Number}) ->
                               SX >= X - 1 andalso SX =< X + Length
                                   andalso SY >= Y - 1 andalso SY =< Y + 1
          end, Numbers),
        case AdjacentNumbers of
            [{_, _, _, Number1}, {_, _, _, Number2}] ->
                [{SX, SY, Symbol, Number1 * Number2} | GearRatios];
            _ -> GearRatios
        end
    end.

is_possible_gear({_X, _Y, Symbol}) -> Symbol =:= <<"*">>.

parse_schematic(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    {_X, _Y, Numbers, Symbols} = lists:foldl(fun parse_line/2, {0, 0, [], []}, Lines),
    {Numbers, Symbols}.

parse_line(<<>>, {_X, Y, Numbers, Symbols}) -> {0, Y + 1, Numbers, Symbols};
parse_line(<<".", Rest/binary>>, {X, Y, Numbers, Symbols}) ->
    parse_line(Rest, {X + 1, Y, Numbers, Symbols});
parse_line(<<N, _/binary>>=Line, {X, Y, Numbers, Symbols}) when N >= $0, N =< $9 ->
    {NumberBin, Trailing} = string:take(Line, lists:seq($0, $9)),
    Length = byte_size(NumberBin),
    Number = {X, Y, Length, binary_to_integer(NumberBin)},
    parse_line(Trailing, {X + Length, Y, [Number | Numbers], Symbols});
parse_line(<<S:1/binary, Rest/binary>>, {X, Y, Numbers, Symbols}) ->
    Symbol = {X, Y, S},
    parse_line(Rest, {X + 1, Y, Numbers, [Symbol | Symbols]}).
