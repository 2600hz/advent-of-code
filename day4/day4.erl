-module(day4).
-export([num_of_matching_pwds1/0, num_of_matching_pwds2/0]).

read_input(InputFile) ->
    {ok, Input} = file:read_file(InputFile),
    BinaryInput = binary:part(Input, {0, byte_size(Input) - 1}),
    << BinaryBeginning:6/binary, "-", BinaryEnd/binary>> = BinaryInput,
    {binary_to_integer(BinaryBeginning), binary_to_integer(BinaryEnd)}.

two_adjacent_digits(X) ->
    [LastDigit | RemainingDigits] = integer_to_list(X),
    two_adjacent_digits(RemainingDigits, LastDigit).

two_adjacent_digits([LastDigit | _], LastDigit) ->
    true;
two_adjacent_digits([CurrentDigit | RemainingDigits], _) ->
    two_adjacent_digits(RemainingDigits, CurrentDigit);
two_adjacent_digits([], _) ->
    false.

adjacent_digits(X) ->
    [LastDigit | RemainingDigits] = integer_to_list(X),
    adjacent_digits(RemainingDigits, LastDigit, #{}).

adjacent_digits([LastDigit | RemainingDigits], LastDigit, AdjacentDigits) ->
    case maps:get(LastDigit, AdjacentDigits, 'undefined') of
        'undefined' -> adjacent_digits(RemainingDigits, LastDigit, AdjacentDigits#{ LastDigit => 2});
        N           -> adjacent_digits(RemainingDigits, LastDigit, AdjacentDigits#{ LastDigit => N + 1})
    end;
adjacent_digits([CurrentDigit | RemainingDigits], _, AdjacentDigits) ->
    adjacent_digits(RemainingDigits, CurrentDigit, AdjacentDigits);
adjacent_digits([], _, AdjacentDigits) ->
    AdjacentDigits.

adjacent_pwd_requirement(Num) ->
    lists:any(fun(X) -> X =:= 2 end, maps:values(adjacent_digits(Num))).

increasing_digits(X) ->
    [LastDigit | RemainingDigits] = integer_to_list(X),
    increasing_digits(RemainingDigits, LastDigit).

increasing_digits([CurrentDigit | RemainingDigits], LastDigit) when CurrentDigit >= LastDigit ->
    increasing_digits(RemainingDigits, CurrentDigit);
increasing_digits([_ | _], _) ->
    false;
increasing_digits([], _) ->
    true.

num_of_matching_pwds1() ->
    {Beginning, End} = read_input("day4.input"),
    length([X || X <- lists:seq(Beginning, End), two_adjacent_digits(X), increasing_digits(X)]).

num_of_matching_pwds2() ->
    {Beginning, End} = read_input("day4.input"),
    length([X || X <- lists:seq(Beginning, End), adjacent_pwd_requirement(X), increasing_digits(X)]).
