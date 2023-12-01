-module(day1).

-export([sum_calibration_values/2]).

sum_calibration_values(Input, Part) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    CalibrationValues = lists:map(calibration_values_mapper(Part), Lines),
    lists:sum(CalibrationValues).

calibration_values_mapper(Part) ->
    fun(Line) -> calibration_values(Line, Part) end.

calibration_values(Line, Part) ->
    {First, Last} = calibration_digits(Line, Part),
    list_to_integer([First, Last]).

calibration_digits(Line, 1) ->
    [H|_T] = Numbers = [X || <<X>> <= Line, X >= $0, X =< $9],
    {H, lists:last(Numbers)};
calibration_digits(Line, 2) ->
    Line1 = convert_spelled_digits(Line),
    calibration_digits(Line1, 1).

convert_spelled_digits(Line) ->
    Replacements = [
        {<<"one">>, 1},
        {<<"two">>, 2},
        {<<"three">>, 3},
        {<<"four">>, 4},
        {<<"five">>, 5},
        {<<"six">>, 6},
        {<<"seven">>, 7},
        {<<"eight">>, 8},
        {<<"nine">>, 9}
    ],
    convert_spelled_digits(Replacements, Line).

convert_spelled_digits([], Line) -> Line;
convert_spelled_digits([Replacement | T]=Replacements, Line) ->
    case convert_spelled_digit(Replacement, Line) of
        'nomatch' -> convert_spelled_digits(T, Line);
        {'match', Line1} ->
            %% We need to start over because the replacement might have more
            %% matches.
            convert_spelled_digits(Replacements, Line1)
    end.

convert_spelled_digit({Spelled, Digit}, Line) ->
    case binary:match(Line, Spelled) of
        'nomatch' -> 'nomatch';
        {Start, _Length} ->
            %% Cannot overwrite the text for the digit because it might be
            %% part of another spelled digit. Instead, break up the spelled
            %% digit we just dealt with so it doesn't get picked up on the next
            %% iteration.
            Prefix = binary:part(Line, 0, Start + 1),
            DigitBin = integer_to_binary(Digit),
            Suffix = binary:part(Line, Start + 1, byte_size(Line) - Start - 1),
            {'match', <<Prefix/binary, DigitBin/binary, Suffix/binary>>}
    end.
