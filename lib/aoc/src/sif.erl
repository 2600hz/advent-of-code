-module(sif).

-export([from_binary/3
        ,to_binary/1
        ,pp/1, pp/3
        ]).
-export([count_digits_in_layer/2]).
-export([layers/1]).
-export([flatten/1]).

-type width() :: pos_integer().
-type height() :: pos_integer().

-type row() :: binary().
-type rows() :: [row()].

-type layer() :: rows().
-type layers() :: [layer()].

-type image() :: {'sif', width(), height(), layers()}.

from_binary(Width, Height, ImageData) ->
    {'sif', Width, Height, image_data_to_layers(Width, Height, ImageData, [])}.

to_binary({'sif', _W, _H, Layers}) ->
    lists:foldl(fun layer_to_binary/2, <<>>, Layers).

-spec pp(image()) -> 'ok'.
pp(Image) ->
    pp(Image, $b, $w).

pp({'sif', _W, _H, Layers}, Black, White) ->
    lists:foreach(fun(Layer) ->  pp_layer(Layer, Black, White) end, Layers).

pp_layer(Rows, Black, White) ->
    lists:foreach(fun(Row) -> pp_row(Row, Black, White) end, Rows).

pp_row(Row, Black, White) ->
    lists:foreach(fun(Pixel) -> pp_pixel(Pixel, Black, White) end, binary_to_list(Row)),
    io:format("~n").

pp_pixel($0, Black, _White) ->
    io:format("~s", [[Black]]);
pp_pixel($1, _Black, White) ->
    io:format("~s", [[White]]).

layer_to_binary(Rows, Acc) ->
    lists:foldl(fun row_to_binary/2, Acc, Rows).

row_to_binary(Row, Acc) ->
    <<Acc/binary, Row/binary>>.

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

-spec flatten(image()) -> image().
flatten({'sif', Width, Height, Layers}) ->
    FlattenedLayer = flatten_layers(Layers),
    {'sif', Width, Height, [FlattenedLayer]}.

-spec flatten_layers(layers()) -> layer().
flatten_layers(Layers) ->
    GroupedLayers = group_by_row(Layers), %% each "layer" is the same row of each layer
    flatten_grouped_layers(GroupedLayers).

flatten_grouped_layers(GroupedLayers) ->
    FlattenedLayer = lists:foldl(fun flatten_grouped_rows/2, [], GroupedLayers),
    lists:reverse(FlattenedLayer).

%% GroupedRows = [<<"00">>,<<"22">>,<<"11">>,<<"02">>]
flatten_grouped_rows([Row], FlattenedLayer) ->
    [Row | FlattenedLayer];
flatten_grouped_rows([TopRow, NextRow | Rows], FlattenedLayer) ->
    Row = flatten_rows(binary_to_list(TopRow), binary_to_list(NextRow)),
    flatten_grouped_rows([Row | Rows], FlattenedLayer).

flatten_rows(TopRow, NextRow) ->
    flatten_rows(TopRow, NextRow, []).

flatten_rows([], [], FlattenedRow) ->
    list_to_binary(lists:reverse(FlattenedRow));
flatten_rows([TopPixel | TopPixels], [NextPixel | NextPixels], FlattenedRow) ->
    Visible = visible_pixel(TopPixel, NextPixel),
    flatten_rows(TopPixels, NextPixels, [Visible | FlattenedRow]).

visible_pixel($2, Next) -> Next;
visible_pixel(Top, _Next) -> Top.

group_by_row(Layers) ->
    group_by_row(Layers, []).

group_by_row([], Grouped) -> lists:reverse(Grouped);
group_by_row(Layers, Grouped) ->
    case lists:foldl(fun group_by_row_fold/2
                    ,{[], []}
                    ,[{Row, Rows} || [Row | Rows] <- Layers]
                    )
    of
        {NewRow, [[] | _]} ->
            group_by_row([], [lists:reverse(NewRow) | Grouped]);
        {NewRow, RowsLeft} ->
            group_by_row(lists:reverse(RowsLeft), [lists:reverse(NewRow) | Grouped])
    end.

group_by_row_fold({Row, Rows}, {NewRow, RowsLeft}) ->
    {[Row | NewRow], [Rows | RowsLeft]}.
