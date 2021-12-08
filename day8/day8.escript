#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 8: Seven Segment Search ---

%% You barely reach the safety of the cave when the whale smashes into
%% the cave mouth, collapsing it. Sensors indicate another exit to
%% this cave at a much greater depth, so you have no choice but to
%% press on.

%% As your submarine slowly makes its way through the cave system, you
%% notice that the four-digit seven-segment displays in your submarine
%% are malfunctioning; they must have been damaged during the
%% escape. You'll be in a lot of trouble without them, so you'd better
%% figure out what's wrong.

%% Each digit of a seven-segment display is rendered by turning on or
%% off any of seven segments named a through g:

%%   0:      1:      2:      3:      4:
%%  aaaa    ....    aaaa    aaaa    ....
%% b    c  .    c  .    c  .    c  b    c
%% b    c  .    c  .    c  .    c  b    c
%%  ....    ....    dddd    dddd    dddd
%% e    f  .    f  e    .  .    f  .    f
%% e    f  .    f  e    .  .    f  .    f
%%  gggg    ....    gggg    gggg    ....

%%   5:      6:      7:      8:      9:
%%  aaaa    aaaa    aaaa    aaaa    aaaa
%% b    .  b    .  .    c  b    c  b    c
%% b    .  b    .  .    c  b    c  b    c
%%  dddd    dddd    ....    dddd    dddd
%% .    f  e    f  .    f  e    f  .    f
%% .    f  e    f  .    f  e    f  .    f
%%  gggg    gggg    ....    gggg    gggg

%% So, to render a 1, only segments c and f would be turned on; the
%% rest would be off. To render a 7, only segments a, c, and f would
%% be turned on.

%% The problem is that the signals which control the segments have
%% been mixed up on each display. The submarine is still trying to
%% display numbers by producing output on signal wires a through g,
%% but those wires are connected to segments randomly. Worse, the
%% wire/segment connections are mixed up separately for each
%% four-digit display! (All of the digits within a display use the
%% same connections, though.)

%% So, you might know that only signal wires b and g are turned on,
%% but that doesn't mean segments b and g are turned on: the only
%% digit that uses two segments is 1, so it must mean segments c and f
%% are meant to be on. With just that information, you still can't
%% tell which wire (b/g) goes to which segment (c/f). For that, you'll
%% need to collect more information.

%% For each display, you watch the changing signals for a while, make
%% a note of all ten unique signal patterns you see, and then write
%% down a single four digit output value (your puzzle input). Using
%% the signal patterns, you should be able to work out which pattern
%% corresponds to which digit.

%% For example, here is what you might see in a single entry in your
%% notes:

%% acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf

%% Each entry consists of ten unique signal patterns, a | delimiter,
%% and finally the four digit output value. Within an entry, the same
%% wire/segment connections are used (but you don't know what the
%% connections actually are). The unique signal patterns correspond to
%% the ten different ways the submarine tries to render a digit using
%% the current wire/segment connections. Because 7 is the only digit
%% that uses three segments, dab in the above example means that to
%% render a 7, signal lines d, a, and b are on. Because 4 is the only
%% digit that uses four segments, eafb means that to render a 4,
%% signal lines e, a, f, and b are on.

%% Using this information, you should be able to work out which
%% combination of signal wires corresponds to each of the ten
%% digits. Then, you can decode the four digit output
%% value. Unfortunately, in the above example, all of the digits in
%% the output value (cdfeb fcadb cdfeb cdbaf) use five segments and
%% are more difficult to deduce.

%% For now, focus on the easy digits. Consider this larger example:

%% be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
%% edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
%% fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
%% fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
%% aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
%% fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
%% dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
%% bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
%% egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
%% gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce

%% Because the digits 1, 4, 7, and 8 each use a unique number of
%% segments, you should be able to tell which combinations of signals
%% correspond to those digits. Counting only digits in the output
%% values (the part after | on each line), in the above example, there
%% are 26 instances of digits that use a unique number of segments
%% (highlighted above).

%% In the output values, how many times do digits 1, 4, 7, or 8
%% appear?

%% --- Part Two ---

%% Through a little deduction, you should now be able to determine the
%% remaining digits. Consider again the first example above:

%% acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf

%% After some careful analysis, the mapping between signal wires and
%% segments only make sense in the following configuration:

%%  dddd
%% e    a
%% e    a
%%  ffff
%% g    b
%% g    b
%%  cccc

%% So, the unique signal patterns would correspond to the following
%% digits:

%%     acedgfb: 8
%%     cdfbe: 5
%%     gcdfa: 2
%%     fbcad: 3
%%     dab: 7
%%     cefabd: 9
%%     cdfgeb: 6
%%     eafb: 4
%%     cagedb: 0
%%     ab: 1

%% Then, the four digits of the output value can be decoded:

%%     cdfeb: 5
%%     fcadb: 3
%%     cdfeb: 5
%%     cdbaf: 3

%% Therefore, the output value for this entry is 5353.

%% Following this same process for each entry in the second, larger
%% example above, the output value of each entry can be determined:

%%     fdgacbe cefdb cefbgd gcbe: 8394
%%     fcgedb cgb dgebacf gc: 9781
%%     cg cg fdcagb cbg: 1197
%%     efabcd cedba gadfec cb: 9361
%%     gecf egdcabf bgf bfgea: 4873
%%     gebdcfa ecba ca fadegcb: 8418
%%     cefg dcbef fcge gbcadfe: 4548
%%     ed bcgafe cdgba cbgef: 1625
%%     gbdfcae bgc cg cgb: 8717
%%     fgae cfgab fg bagce: 4315

%% Adding all of the output values in this larger example produces
%% 61229.

%% For each entry, determine all of the wire/segment connections and
%% decode the four-digit output values. What do you get if you add up
%% all of the output values?

main(_) ->
    Input = read_input("p8-test.txt"),
    p8_1(Input),
    p8_2(Input).

p8_1(Input) ->
    Counts = lists:foldl(fun count_easy/2, 0, Input),
    io:format("easy digits: ~p~n", [Counts]).

count_easy({_SignalPatterns, OutputValues}, Count) ->
    lists:foldl(fun count_easy_digit/2, Count, OutputValues).

%% 1 - two segments used
count_easy_digit(<<_:2/binary>>, Count) ->
    Count+1;
%% 4 - four segments used
count_easy_digit(<<_:4/binary>>, Count) ->
    Count+1;
%% 7 - three segments used
count_easy_digit(<<_:3/binary>>, Count) ->
    Count+1;
%% 8 - seven segments used
count_easy_digit(<<_:7/binary>>, Count) ->
    Count+1;
count_easy_digit(_Digit, Count) -> Count.

p8_2(Input) ->
    Decoded = lists:foldl(fun decode_digits/2, [], Input),
    Sum = lists:sum(Decoded),
    io:format("sum of decoded: ~p~n~p~n", [Sum, Decoded]).

decode_digits({Signals, OutputValues}, Decoded) ->
    {TryAgainSignals, SegmentMapping} = map_signal_to_segment(Signals, #{}),
    io:format("signals: ~p~noutputs: ~p~n", [Signals, OutputValues]),
    io:format("map: ~p~ntry: ~p~n~n", [maps:map(fun from_set/2, SegmentMapping), TryAgainSignals]),
    Decoded.

from_set(_Segment, Possible) ->
    sets:to_list(Possible).

%% a digit is:
%%  aaaa
%% b    c
%% b    c
%%  dddd
%% e    f
%% e    f
%%  gggg
%% so we map "easy" digits (like if 1 is "eg", "e"=>c, "g"=>f, then if
%% we see "g" somewhere else, we know it maps to the original
%% diagram's "f" segment).
map_signal_to_segment(Signals, Mapping) ->
    map_signal_to_segment(Signals, [], Mapping).

map_signal_to_segment([], TryAgainSignals, Mapping) ->
    {TryAgainSignals, Mapping};
map_signal_to_segment([<<_:2/binary>> = One | Signals], TryAgainSignals, Mapping) ->
    Possible = sets:from_list(binary_to_list(One)),
    Mapping1 = add_to_mapping(Mapping, [{<<"c">>, Possible}
                                       ,{<<"f">>, Possible}
                                       ]),
    map_signal_to_segment(Signals, TryAgainSignals, Mapping1);
map_signal_to_segment([<<_:3/binary>> = Seven | Signals], TryAgainSignals, Mapping) ->
    Possible = sets:from_list(binary_to_list(Seven)),
    Mapping1 = add_to_mapping(Mapping, [{<<"a">>, Possible}
                                       ,{<<"c">>, Possible}
                                       ,{<<"f">>, Possible}
                                       ]),
    map_signal_to_segment(Signals, TryAgainSignals, Mapping1);
map_signal_to_segment([<<_:4/binary>> = Four | Signals], TryAgainSignals, Mapping) ->
    Possible = sets:from_list(binary_to_list(Four)),
    Mapping1 = add_to_mapping(Mapping, [{<<"b">>, Possible}
                                       ,{<<"c">>, Possible}
                                       ,{<<"d">>, Possible}
                                       ,{<<"f">>, Possible}
                                       ]),
    map_signal_to_segment(Signals, TryAgainSignals, Mapping1);
map_signal_to_segment([<<_:7/binary>> = Eight | Signals], TryAgainSignals, Mapping) ->
    Possible = sets:from_list(binary_to_list(Eight)),
    Mapping1 = add_to_mapping(Mapping, [{<<"a">>, Possible}
                                       ,{<<"b">>, Possible}
                                       ,{<<"c">>, Possible}
                                       ,{<<"d">>, Possible}
                                       ,{<<"e">>, Possible}
                                       ,{<<"f">>, Possible}
                                       ,{<<"g">>, Possible}
                                       ]),
    map_signal_to_segment(Signals, TryAgainSignals, Mapping1);
map_signal_to_segment([Signal | Signals], TryAgainSignals, Mapping) ->
    map_signal_to_segment(Signals, [Signal | TryAgainSignals], Mapping).

add_to_mapping(Mapping, []) -> Mapping;
add_to_mapping(Mapping, [{Segment, SignalWires} | Outputs]) ->
    case maps:get(Segment, Mapping, 'undefined') of
        'undefined' ->
            add_to_mapping(Mapping#{Segment => SignalWires}, Outputs);
        SWs ->
            add_to_mapping(Mapping#{Segment => sets:intersection(SWs, SignalWires)}
                          ,Outputs
                          )
    end.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [process_line(Line) || Line <- binary:split(Lines, <<"\n">>, ['global']), Line =/= <<>>].

process_line(Line) ->
    [SignalPatterns, OutputValue] = binary:split(Line, <<" | ">>),
    {binary:split(SignalPatterns, <<" ">>, ['global'])
    ,binary:split(OutputValue, <<" ">>, ['global'])
    }.

signals: [<<"be">>,<<"cfbegad">>,<<"cbdgef">>,<<"fgaecd">>,<<"cgeb">>,
          <<"fdcge">>,<<"agebfd">>,<<"fecdb">>,<<"fabcd">>,<<"edb">>]
[lists:sort(binary_to_list(Signal)) || Signal <- Signals] =>

["be" => 1 => "ac"
,"abcdefg"
,"bcdefg"
,"acdefg"
,"bceg" => 4 => "bcdf"
,"cdefg"
,"abdefg"
,"bcdef"
,"abcdf"
,"bde" => 7 => "acf"
]

sets:to_list(sets:subtract(sets:from_list("bde"), sets:from_list("be"))).
"d"

model => signal
"a"  =>   "d"

#{0 => "abcefg"
 ,1 => "ac"
 ,2 => "acdeg"
 ,3 => "acdfg"
 ,4 => "bcdf"
 ,5 => "abdfg"
 ,6 => "abdefg"
 ,7 => "acf"
 ,8 => "abcdefg"
 ,9 => "abcdfg"
 }

outputs: [<<"fdgacbe">>,<<"cefdb">>,<<"cefbgd">>,<<"gcbe">>]
map: #{<<"a">> => "bed",<<"b">> => "cbeg",<<"c">> => "be",<<"d">> => "cbeg",
       <<"e">> => "cfbeadg",<<"f">> => "be",<<"g">> => "cfbeadg"}
try: [<<"fabcd">>,<<"fecdb">>,<<"agebfd">>,<<"fdcge">>,<<"fgaecd">>,
      <<"cbdgef">>]
