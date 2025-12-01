-module(aoc).

-export([day1/1]).

day1(InputType) ->
    Lines = read_puzzle_input("day1", InputType),
    Rotations = parse_rotations(Lines),
    StartPoint = 50,
    {Zeros, EndPoint} = lists:foldl(fun rotate_dial/2, {0, StartPoint}, Rotations),

    {SecureZeros, EndPoint} = lists:foldl(fun rotate_dial_0x434C49434B/2, {0, StartPoint}, Rotations),
    io:format("part1:~n  ended at point ~p, hit ~p zeros~n", [EndPoint, Zeros]),
    io:format("part2:~n  ended at point ~p, passed ~p zeros~n", [EndPoint, SecureZeros]).

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

rotate_dial_0x434C49434B({Op, Turns}, {Zeros, Point}) ->
    {NewPoint, Passes} = normalize_point_0x434C49434B(Point, Op(Point, Turns)),
    {Zeros + Passes, NewPoint}.

normalize_point_0x434C49434B(Point, NewPoint) ->
    normalize_point_0x434C49434B(Point, NewPoint, 0).

normalize_point_0x434C49434B(_Point, 0, Passes) ->
    {0, Passes+1};
normalize_point_0x434C49434B(_Point, 100, Passes) ->
    {0, Passes+1};
normalize_point_0x434C49434B(0, Negative, Passes) when Negative < 0 ->
    normalize_point_0x434C49434B(Negative, Negative+100, Passes);
normalize_point_0x434C49434B(_Point, Negative, Passes) when Negative < 0 ->
    normalize_point_0x434C49434B(Negative, Negative+100, Passes+1);
normalize_point_0x434C49434B(_Point, Over99, Passes) when Over99 > 99 ->
    normalize_point_0x434C49434B(Over99, Over99 - 100, Passes+1);
normalize_point_0x434C49434B(_Point, NewPoint, Passes) -> {NewPoint, Passes}.

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
