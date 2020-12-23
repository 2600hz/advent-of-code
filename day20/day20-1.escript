#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 20: Jurassic Jigsaw ---

%% The high-speed train leaves the forest and quickly carries you
%% south. You can even see a desert in the distance! Since you have
%% some spare time, you might as well see if there was anything
%% interesting in the image the Mythical Information Bureau satellite
%% captured.

%% After decoding the satellite messages, you discover that the data
%% actually contains many small images created by the satellite's
%% camera array. The camera array consists of many cameras; rather than
%% produce a single square image, they produce many smaller square
%% image tiles that need to be reassembled back into a single image.

%% Each camera in the camera array returns a single monochrome image
%% tile with a random unique ID number. The tiles (your puzzle input)
%% arrived in a random order.

%% Worse yet, the camera array appears to be malfunctioning: each image
%% tile has been rotated and flipped to a random orientation. Your
%% first task is to reassemble the original image by orienting the
%% tiles so they fit together.

%% To show how the tiles should be reassembled, each tile's image data
%% includes a border that should line up exactly with its adjacent
%% tiles. All tiles have this border, and the border lines up exactly
%% when the tiles are both oriented correctly. Tiles at the edge of the
%% image also have this border, but the outermost edges won't line up
%% with any other tiles.

%% For example, suppose you have the following nine tiles:

%% Tile 2311:
%% ..##.#..#.
%% ##..#.....
%% #...##..#.
%% ####.#...#
%% ##.##.###.
%% ##...#.###
%% .#.#.#..##
%% ..#....#..
%% ###...#.#.
%% ..###..###

%% Tile 1951:
%% #.##...##.
%% #.####...#
%% .....#..##
%% #...######
%% .##.#....#
%% .###.#####
%% ###.##.##.
%% .###....#.
%% ..#.#..#.#
%% #...##.#..

%% Tile 1171:
%% ####...##.
%% #..##.#..#
%% ##.#..#.#.
%% .###.####.
%% ..###.####
%% .##....##.
%% .#...####.
%% #.##.####.
%% ####..#...
%% .....##...

%% Tile 1427:
%% ###.##.#..
%% .#..#.##..
%% .#.##.#..#
%% #.#.#.##.#
%% ....#...##
%% ...##..##.
%% ...#.#####
%% .#.####.#.
%% ..#..###.#
%% ..##.#..#.

%% Tile 1489:
%% ##.#.#....
%% ..##...#..
%% .##..##...
%% ..#...#...
%% #####...#.
%% #..#.#.#.#
%% ...#.#.#..
%% ##.#...##.
%% ..##.##.##
%% ###.##.#..

%% Tile 2473:
%% #....####.
%% #..#.##...
%% #.##..#...
%% ######.#.#
%% .#...#.#.#
%% .#########
%% .###.#..#.
%% ########.#
%% ##...##.#.
%% ..###.#.#.

%% Tile 2971:
%% ..#.#....#
%% #...###...
%% #.#.###...
%% ##.##..#..
%% .#####..##
%% .#..####.#
%% #..#.#..#.
%% ..####.###
%% ..#.#.###.
%% ...#.#.#.#

%% Tile 2729:
%% ...#.#.#.#
%% ####.#....
%% ..#.#.....
%% ....#..#.#
%% .##..##.#.
%% .#.####...
%% ####.#.#..
%% ##.####...
%% ##..#.##..
%% #.##...##.

%% Tile 3079:
%% #.#.#####.
%% .#..######
%% ..#.......
%% ######....
%% ####.#..#.
%% .#...#.##.
%% #.#####.##
%% ..#.###...
%% ..#.......
%% ..#.###...

%% By rotating, flipping, and rearranging them, you can find a square
%% arrangement that causes all adjacent borders to line up:

%% #...##.#.. ..###..### #.#.#####.
%% ..#.#..#.# ###...#.#. .#..######
%% .###....#. ..#....#.. ..#.......
%% ###.##.##. .#.#.#..## ######....
%% .###.##### ##...#.### ####.#..#.
%% .##.#....# ##.##.###. .#...#.##.
%% #...###### ####.#...# #.#####.##
%% .....#..## #...##..#. ..#.###...
%% #.####...# ##..#..... ..#.......
%% #.##...##. ..##.#..#. ..#.###...

%% #.##...##. ..##.#..#. ..#.###...
%% ##..#.##.. ..#..###.# ##.##....#
%% ##.####... .#.####.#. ..#.###..#
%% ####.#.#.. ...#.##### ###.#..###
%% .#.####... ...##..##. .######.##
%% .##..##.#. ....#...## #.#.#.#...
%% ....#..#.# #.#.#.##.# #.###.###.
%% ..#.#..... .#.##.#..# #.###.##..
%% ####.#.... .#..#.##.. .######...
%% ...#.#.#.# ###.##.#.. .##...####

%% ...#.#.#.# ###.##.#.. .##...####
%% ..#.#.###. ..##.##.## #..#.##..#
%% ..####.### ##.#...##. .#.#..#.##
%% #..#.#..#. ...#.#.#.. .####.###.
%% .#..####.# #..#.#.#.# ####.###..
%% .#####..## #####...#. .##....##.
%% ##.##..#.. ..#...#... .####...#.
%% #.#.###... .##..##... .####.##.#
%% #...###... ..##...#.. ...#..####
%% ..#.#....# ##.#.#.... ...##.....

%% For reference, the IDs of the above tiles are:

%% 1951    2311    3079
%% 2729    1427    2473
%% 2971    1489    1171

%% To check that you've assembled the image correctly, multiply the IDs
%% of the four corner tiles together. If you do this with the assembled
%% tiles from the example above, you get 1951 * 3079 * 2971 * 1171 =
%% 20899048083289.

%% Assemble the tiles into an image. What do you get if you multiply
%% together the IDs of the four corner tiles?

main(_) ->
    Tiles = read_input("p20.txt"),
    Borders = find_borders(Tiles),
    CornerProduct = maps:fold(fun(Corner, [_, _], Prod) ->
                                      Corner * Prod;
                                 (_, _, Prod) -> Prod
                              end
                             ,1
                             ,find_corners(Borders)
                             ),
    io:format("product: ~p~n", [CornerProduct]).

find_corners(Borders) ->
    maps:fold(fun is_corner/3, #{}, Borders).

is_corner(_Edge, [{Id1, _Orient1}, {Id2, _Orient2}], Corners) ->
    Corners#{Id1 => lists:usort([Id2 | maps:get(Id1, Corners, [])])
           ,Id2 => lists:usort([Id1 | maps:get(Id2, Corners, [])])
           };
is_corner(_Edge, _Orients, Corners) ->
    Corners.

find_borders(Tiles) ->
    maps:fold(fun find_borders/3, #{}, Tiles).

find_borders(TileId, Tile, Borders) ->
    TileBorders = tile_borders(Tile),
    lists:foldl(fun({Orientation, TileBorder}, Bs) ->
                        Border = list_to_binary(TileBorder),
                        RevBorder = list_to_binary(lists:reverse(TileBorder)),

                        BorderTileIds = maps:get(Border, Bs, []),
                        RevBorderTileIds = maps:get(RevBorder, Bs, []),

                        Bs#{Border => [{TileId, Orientation} | BorderTileIds]
                          ,RevBorder => [{TileId, Orientation} | RevBorderTileIds]
                          }
                end
               ,Borders
               ,TileBorders
               ).

%% [ [{x,y,cell}, {x+1,y,cell}...],...]
tile_borders(Tile) ->
    [{'north', edge(Tile, fun north_edge/1)}
    ,{'east', edge(Tile, fun east_edge/1)}
    ,{'south', edge(Tile, fun south_edge/1)}
    ,{'west', edge(Tile, fun west_edge/1)}
    ].

north_edge({_X, Y, Cell}) ->
    case Y =:= 0 of
        'true' -> {'true', Cell};
        'false' -> 'false'
    end.

south_edge({_X, Y, Cell}) ->
    case Y =:= 9 of
        'true' -> {'true', Cell};
        'false' -> 'false'
    end.

west_edge({X, _Y, Cell}) ->
    case X =:= 0 of
        'true' -> {'true', Cell};
        'false' -> 'false'
    end.

east_edge({X, _Y, Cell}) ->
    case X =:= 9 of
        'true' -> {'true', Cell};
        'false' -> 'false'
    end.

edge(Tile, FilterMap) ->
    lists:filtermap(FilterMap, Tile).

read_input(Filename) ->
    {'ok', File} = file:read_file(Filename),
    read_tiles(binary:split(File, <<"\n">>, ['global', 'trim'])).

read_tiles([TileLine | Lines]) ->
    {Tiles, LastTileId} = lists:foldl(fun read_tile/2
                                     ,{#{}, parse_tile_id(TileLine)}
                                     ,Lines
                                     ),
    Tile = maps:get(LastTileId, Tiles),
    Tiles#{LastTileId => tile_coordinates(Tile)}.

parse_tile_id(<<"Tile ", TileColon/binary>>) ->
    TileIdSize = byte_size(TileColon)-1,
    <<TileId:TileIdSize/binary, _/binary>> = TileColon,
    binary_to_integer(TileId, 10).

read_tile(<<>>, Acc) -> Acc;
read_tile(<<"Tile ", _/binary>>=TileLine, {Tiles, TileId}) ->
    Tile = maps:get(TileId, Tiles),
    {Tiles#{TileId => tile_coordinates(Tile)}, parse_tile_id(TileLine)};
read_tile(TileLine, {Tiles, TileId}) ->
    Tile = maps:get(TileId, Tiles, []),

    {Tiles#{TileId => [[ <<C>> || <<C>> <= TileLine ] | Tile]}, TileId}.

tile_coordinates(Tile) ->
    {TileCo, _Y} =
        lists:foldl(fun tile_line_coordinates/2
                   ,{[], 0}
                   ,lists:reverse(Tile)
                   ),
    TileCo.

tile_line_coordinates(Cells, {Coordinates, Y}) ->
    {Cs, _, _} = lists:foldl(fun add_cell/2, {Coordinates, Y, 0}, Cells),
    {Cs, Y+1}.

add_cell(Cell, {Coordinates, Y, X}) ->
    {[{X, Y, Cell} | Coordinates], Y, X+1}.
