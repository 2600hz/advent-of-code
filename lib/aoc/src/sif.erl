-module(sif).

-export([from_binary/3]).
-export([count_digits_in_layer/2]).
-export([layers/1]).

-type width() :: pos_integer().
-type height() :: pos_integer().

-type row() :: binary().
-type rows() :: [row()].

-type layer() :: rows().
-type layers() :: [layer()].

-type image() :: {'sif', width(), height(), layers()}.

from_binary(Width, Height, ImageData) ->
    {'sif', Width, Height, image_data_to_layers(Width, Height, ImageData, [])}.

-spec layers(image()) -> layers().
layers({'sif', _W, _H, Layers}) -> Layers.

image_data_to_layers(_Width, _Height, <<>>, Layers) ->
    lists:reverse(Layers);
image_data_to_layers(_Width, _Height, <<"\n">>, Layers) ->
    lists:reverse(Layers);
image_data_to_layers(Width, Height, ImageData, Layers) ->
    {Layer, RestImageData} = read_layer(Width, Height, ImageData),
    image_data_to_layers(Width, Height, RestImageData, [Layer | Layers]).

read_layer(Width, Height, ImageData) ->
    Pixels = Width * Height,
    <<Layer:Pixels/binary, RestImageData/binary>> = ImageData,
    {layer_to_rows(Width, Layer), RestImageData}.

layer_to_rows(Width, Layer) ->
    layer_to_rows(Width, Layer, []).

layer_to_rows(_Width, <<>>, Rows) ->
    lists:reverse(Rows);
layer_to_rows(Width, Layer, Rows) ->
    <<Row:Width/binary, Rest/binary>> = Layer,
    layer_to_rows(Width, Rest, [Row | Rows]).

-spec count_digits_in_row(rows(), non_neg_integer()) -> non_neg_integer().
count_digits_in_layer(Rows, Digit) ->
    {Digit, Count} = lists:foldl(fun count_digits_in_row/2, {Digit, 0}, Rows),
    Count.

count_digits_in_row(Row, {Digit, Sum}) ->
    DigitAscii = $0+Digit,
    Zeros = << <<Digit>> || <<Pixel>> <= Row, Pixel =:= DigitAscii >>,
    {Digit, byte_size(Zeros) + Sum}.
