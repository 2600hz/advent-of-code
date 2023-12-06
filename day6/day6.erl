-module(day6).

-export([record_ways_product/2]).

record_ways_product(Input, Part) ->
    Races = parse_races(Input, Part),
    RacesWithBounds = [add_bounds(Race) || Race <- Races],
    Ranges = [Range || {_Time, _Record, _Lower, _Upper, Range} <- RacesWithBounds],
    lists:foldl(fun 'erlang':'*'/2, 1, Ranges).

add_bounds({Time, Record}) ->
    Lower = floor(quadratic(1, -Time, Record, 'lower') + 1),
    Upper = ceil(quadratic(1, -Time, Record, 'upper') - 1),
    Range = Upper - Lower + 1,
    {Time, Record, Lower, Upper, Range}.

quadratic(A, B, C, UpperOrLower) ->
    Oper = case UpperOrLower of
               'upper' -> fun 'erlang':'+'/2;
               'lower' -> fun 'erlang':'-'/2
           end,
    Oper(-B, math:sqrt(math:pow(B, 2) - 4 * A * C)) / (2 * A).

parse_races(Input, Part) ->
    [Time, Distance] = binary:split(Input, <<"\n">>),
    parse_races(Time, Distance, Part).

parse_races(<<"Time:", TimesBin/binary>>
           ,<<"Distance:", RecordsBin/binary>>
           ,1
           ) ->
    Times = binary:split(TimesBin, <<" ">>, ['global', 'trim_all']),
    Records = binary:split(RecordsBin, <<" ">>, ['global', 'trim_all']),
    lists:zip([parse_int(Time) || Time <- Times]
             ,[parse_int(Record) || Record <- Records]
             );
parse_races(Times, Records, 2) ->
    parse_races(remove_spaces(Times), remove_spaces(Records), 1).

parse_int(Bin) -> binary_to_integer(string:trim(Bin)).

remove_spaces(Bin) -> binary:replace(Bin, <<" ">>, <<>>, ['global']).
