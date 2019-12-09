#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Day 8: Space Image Format ---

%% The Elves' spirits are lifted when they realize you have an
%% opportunity to reboot one of their Mars rovers, and so they are
%% curious if you would spend a brief sojourn on Mars. You land your
%% ship near the rover.

%% When you reach the rover, you discover that it's already in the
%% process of rebooting! It's just waiting for someone to enter a BIOS
%% password. The Elf responsible for the rover takes a picture of the
%% password (your puzzle input) and sends it to you via the Digital
%% Sending Network.

%% Unfortunately, images sent via the Digital Sending Network aren't
%% encoded with any normal encoding; instead, they're encoded in a
%% special Space Image Format. None of the Elves seem to remember why
%% this is the case. They send you the instructions to decode it.

%% Images are sent as a series of digits that each represent the color
%% of a single pixel. The digits fill each row of the image
%% left-to-right, then move downward to the next row, filling rows
%% top-to-bottom until every pixel of the image is filled.

%% Each image actually consists of a series of identically-sized
%% layers that are filled in this way. So, the first digit corresponds
%% to the top-left pixel of the first layer, the second digit
%% corresponds to the pixel to the right of that on the same layer,
%% and so on until the last digit, which corresponds to the
%% bottom-right pixel of the last layer.

%% For example, given an image 3 pixels wide and 2 pixels tall, the
%% image data 123456789012 corresponds to the following image layers:

%% Layer 1: 123
%%          456

%% Layer 2: 789
%%          012

%% The image you received is 25 pixels wide and 6 pixels tall.

%% To make sure the image wasn't corrupted during transmission, the
%% Elves would like you to find the layer that contains the fewest 0
%% digits. On that layer, what is the number of 1 digits multiplied by
%% the number of 2 digits?

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    {Width, Height, ImageData} = image_data(),
    Layers = image_data_to_layers(Width, Height, ImageData),
    FewestZeroLayer = find_fewest_zeros(Layers),

    Checksum = count_digits_in_layer(FewestZeroLayer, 1)
        * count_digits_in_layer(FewestZeroLayer, 2),

    io:format("fewest 0s has chksum ~p: ~p~n", [Checksum, FewestZeroLayer]).

find_fewest_zeros([Layer | Layers]) ->
    find_fewest_zeros(Layers, Layer, count_digits_in_layer(Layer, 0)).

find_fewest_zeros([], Layer, _Count) ->
    io:format("fewest zeros: ~p~n", [_Count]),
    Layer;
find_fewest_zeros([Layer | Layers], Fewest, ZeroCount) ->
    case count_digits_in_layer(Layer, 0) of
        EvenLess when EvenLess < ZeroCount ->
            find_fewest_zeros(Layers, Layer, EvenLess);
        _ ->
            find_fewest_zeros(Layers, Fewest, ZeroCount)
    end.

count_digits_in_layer({'incomplete', _Layer}, _Digit) -> 'undefined'; % undefined > 0 in term ordering
count_digits_in_layer(Rows, Digit) ->
    {Digit, Count} = lists:foldl(fun count_digits_in_row/2, {Digit, 0}, Rows),
    Count.

count_digits_in_row(Row, {Digit, Sum}) ->
    DigitAscii = $0+Digit,
    Zeros = << <<Digit>> || <<Pixel>> <= Row, Pixel =:= DigitAscii >>,
    {Digit, byte_size(Zeros) + Sum}.

image_data_to_layers(Width, Height, ImageData) ->
    image_data_to_layers(Width, Height, ImageData, []).

image_data_to_layers(_Width, _Height, <<>>, Layers) ->
    lists:reverse(Layers);
image_data_to_layers(_Width, _Height, <<"\n">>, Layers) ->
    lists:reverse(Layers);
image_data_to_layers(Width, Height, ImageData, Layers) ->
    {Layer, RestImageData} = read_layer(Width, Height, ImageData),
    image_data_to_layers(Width, Height, RestImageData, [Layer | Layers]).

read_layer(Width, Height, ImageData) ->
    Pixels = Width * Height,
    case ImageData of
        <<Layer:Pixels/binary, RestImageData/binary>> ->
            {layer_to_rows(Width, Layer), RestImageData};
        <<Incomplete/binary>> ->
            {{'incomplete', Incomplete}, <<>>}
    end.

layer_to_rows(Width, Layer) ->
    layer_to_rows(Width, Layer, []).

layer_to_rows(_Width, <<>>, Rows) ->
    lists:reverse(Rows);
layer_to_rows(Width, Layer, Rows) ->
    <<Row:Width/binary, Rest/binary>> = Layer,
    layer_to_rows(Width, Rest, [Row | Rows]).

image_data() ->
    ImageData = read_input(),
    {25, 6, binary:replace(ImageData, <<"\n">>, <<>>)}.

read_input() ->
    %% test_input().
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.

%%     test_image_data().

%% test_image_data() ->
%%     {3, 2, <<"123456789012">>}.
