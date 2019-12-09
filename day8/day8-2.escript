#!/usr/bin/env escript
%%! +A2 -pa ../lib/aoc/_build/default/lib/aoc/ebin
%% -*- coding: utf-8 -*-

%% --- Part Two ---

%% Now you're ready to decode the image. The image is rendered by
%% stacking the layers and aligning the pixels with the same positions
%% in each layer. The digits indicate the color of the corresponding
%% pixel: 0 is black, 1 is white, and 2 is transparent.

%% The layers are rendered with the first layer in front and the last
%% layer in back. So, if a given position has a transparent pixel in
%% the first and second layers, a black pixel in the third layer, and
%% a white pixel in the fourth layer, the final image would have a
%% black pixel at that position.

%% For example, given an image 2 pixels wide and 2 pixels tall, the
%% image data 0222112222120000 corresponds to the following image
%% layers:

%% Layer 1: 02
%%          22

%% Layer 2: 11
%%          22

%% Layer 3: 22
%%          12

%% Layer 4: 00
%%          00

%% Then, the full image can be found by determining the top visible
%% pixel in each position:

%%     The top-left pixel is black because the top layer is 0.
%%     The top-right pixel is white because the top layer is 2
%%     (transparent), but the second layer is 1.
%%     The bottom-left pixel is white because the top two layers are
%%     2, but the third layer is 1.
%%     The bottom-right pixel is black because the only visible pixel
%%     in that position is 0 (from layer 4).

%% So, the final image looks like this:

%% 01
%% 10

%% What message is produced after decoding your image?

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    {Width, Height, ImageData} = image_data(),
    SIFImage = sif:from_binary(Width, Height, ImageData),
    FlattenedImage = sif:flatten(SIFImage),
    FlatBin = sif:to_binary(FlattenedImage),
    io:format("sif: ~n~s~n~npp:~n", [FlatBin]),
    sif:pp(FlattenedImage, $., $O).

image_data() ->
    %%     {2, 2, test_input()}.
    {25, 6, binary:replace(read_input(), <<"\n">>, <<>>)}.

read_input() ->
    %% test_input().
    ThisDirectory = filename:dirname(escript:script_name()),
    Input = filename:join([ThisDirectory, "input.txt"]),
    {'ok', Contents} = file:read_file(Input),
    Contents.

%% test_input() ->
%%     <<"0222112222120000">>.
