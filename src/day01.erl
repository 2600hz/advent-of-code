-module(day01).

-export([run/0]).

%% https://adventofcode.com/2023/day/1

%% Part 1: Calibration values summed: 56465
%% Part 2: Calibration values summed: 55902

run() ->
    part1(input("day01.txt")),
    part2(input("day01.txt")).

part1(Input) ->
    Numbers = read_first_last(Input, fun just_number/1),
    Sum = lists:sum(Numbers),
    io:format('user', "Part 1: Calibration values summed: ~p~n", [Sum]).

part2(Input) ->
    Numbers = read_first_last(Input, fun text_to_number/1),
    Sum = lists:sum(Numbers),
    io:format('user', "Part 2: Calibration values summed: ~p~n", [Sum]).

text_to_number(<<>>) -> 'eof';
text_to_number(<<"one", Rest/binary>>) ->
    {<<"e", Rest/binary>>, $1};
text_to_number(<<"two", Rest/binary>>) ->
    {<<"o", Rest/binary>>, $2};
text_to_number(<<"three", Rest/binary>>) ->
    {<<"e", Rest/binary>>, $3};
text_to_number(<<"four", Rest/binary>>) ->
    {<<"r", Rest/binary>>, $4};
text_to_number(<<"five", Rest/binary>>) ->
    {<<"e", Rest/binary>>, $5};
text_to_number(<<"six", Rest/binary>>) ->
    {<<"x", Rest/binary>>, $6};
text_to_number(<<"seven", Rest/binary>>) ->
    {<<"n", Rest/binary>>, $7};
text_to_number(<<"eight", Rest/binary>>) ->
    {<<"t", Rest/binary>>, $8};
text_to_number(<<"nine", Rest/binary>>) ->
    {<<"e", Rest/binary>>, $9};
text_to_number(<<Numeric:8, Rest/binary>>)
  when Numeric > $0, Numeric =< $9 ->
    {Rest, Numeric};
text_to_number(<<$\n, Rest/binary>>) ->
    {Rest, 'eol'};
text_to_number(<<_C:8, Rest/binary>>) ->
    text_to_number(Rest).

just_number(<<>>) -> 'eof';
just_number(<<Numeric:8, Rest/binary>>)
  when Numeric > $0, Numeric =< $9 ->
    {Rest, Numeric};
just_number(<<$\n, Rest/binary>>) ->
    {Rest, 'eol'};
just_number(<<_C:8, Rest/binary>>) ->
    just_number(Rest).

read_first_last(Input, Reader) ->
    read_first_last(Input, Reader, []).

read_first_last(<<>>, _Reader, Numbers) -> Numbers;
read_first_last(Input, Reader, Numbers) ->
    {Rest, First} = Reader(Input),
    read_last(Rest, Reader, Numbers, First).

read_last(Input, Reader, Numbers, First) ->
    read_last(Input, Reader, Numbers, First, First).

read_last(Input, Reader, Numbers, First, Last) ->
    case Reader(Input) of
        'eof' ->
            [(First-$0)*10 + (Last-$0) | Numbers];
        {Rest, 'eol'} ->
            read_first_last(Rest, Reader, [(First-$0)*10 + (Last-$0) | Numbers]);
        {Rest, N} ->
            read_last(Rest, Reader, Numbers, First, N)
    end.

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.
