-module(day01).

-export([run/0]).

%%% --- Day 1: Trebuchet?! ---

%% Something is wrong with global snow production, and you've been
%% selected to take a look. The Elves have even given you a map; on
%% it, they've used stars to mark the top fifty locations that are
%% likely to be having problems.

%% You've been doing this long enough to know that to restore snow
%% operations, you need to check all fifty stars by December 25th.

%% Collect stars by solving puzzles. Two puzzles will be made
%% available on each day in the Advent calendar; the second puzzle is
%% unlocked when you complete the first. Each puzzle grants one
%% star. Good luck!

%% You try to ask why they can't just use a weather machine ("not
%% powerful enough") and where they're even sending you ("the sky")
%% and why your map looks mostly blank ("you sure ask a lot of
%% questions") and hang on did you just say the sky ("of course, where
%% do you think snow comes from") when you realize that the Elves are
%% already loading you into a trebuchet ("please hold still, we need
%% to strap you in").

%% As they're making the final adjustments, they discover that their
%% calibration document (your puzzle input) has been amended by a very
%% young Elf who was apparently just excited to show off her art
%% skills. Consequently, the Elves are having trouble reading the
%% values on the document.

%% The newly-improved calibration document consists of lines of text;
%% each line originally contained a specific calibration value that
%% the Elves now need to recover. On each line, the calibration value
%% can be found by combining the first digit and the last digit (in
%% that order) to form a single two-digit number.

%% For example:

%% 1abc2
%% pqr3stu8vwx
%% a1b2c3d4e5f
%% treb7uchet

%% In this example, the calibration values of these four lines are 12,
%% 38, 15, and 77. Adding these together produces 142.

%% Consider your entire calibration document. What is the sum of all
%% of the calibration values?

%% --- Part Two ---

%% Your calculation isn't quite right. It looks like some of the
%% digits are actually spelled out with letters: one, two, three,
%% four, five, six, seven, eight, and nine also count as valid
%% "digits".

%% Equipped with this new information, you now need to find the real
%% first and last digit on each line. For example:

%% two1nine
%% eightwothree
%% abcone2threexyz
%% xtwone3four
%% 4nineeightseven2
%% zoneight234
%% 7pqrstsixteen

%% In this example, the calibration values are 29, 83, 13, 24, 42, 14,
%% and 76. Adding these together produces 281.

%% What is the sum of all of the calibration values?

%% Answers:
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
