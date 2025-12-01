-module(aoc).

-export([day1/1]).

day1(InputType) ->
    Lines = read_puzzle_input("day1", InputType),
    Rotations = parse_rotations(Lines),
    StartPoint = 50,
    {Zeros, EndPoint} = lists:foldl(fun rotate_dial/2, {0, StartPoint}, Rotations),
    io:format("ended at point ~p, hit ~p zeros~n", [EndPoint, Zeros]).

rotate_dial({Op, Turns}, {Zeros, Point}) ->
    case normalize_point(Op(Point, Turns)) of
        0 -> {Zeros+1, 0};
        NewPoint -> {Zeros, NewPoint}
    end.

normalize_point(Negative) when Negative < 0 ->
    %% -1 => 99, -2 => 98, ...
    normalize_point(Negative + 100);
normalize_point(Over99) when Over99 > 99 ->
    normalize_point(Over99 - 100);
normalize_point(Point) -> Point.

parse_rotations(Lines) ->
    [parse_rotation(Line) || Line <- Lines].

parse_rotation(<<"L", Turns/binary>>) ->
    {fun erlang:'-'/2, binary_to_integer(Turns, 10)};
parse_rotation(<<"R", Turns/binary>>) ->
    {fun erlang:'+'/2, binary_to_integer(Turns, 10)}.

read_puzzle_input(Day, InputType) ->
    PrivDir = code:priv_dir('aoc'),
    InputFile = filename:join([PrivDir, [Day, $., InputType]]),
    {'ok', Puzzle} = file:read_file(InputFile),
    binary:split(Puzzle, <<"\n">>, ['global', 'trim']).
