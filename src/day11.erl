-module(day11).

-export([run/0]).

%% https://adventofcode.com/2023/day/11/
%% galaxy distances: 9957702
%% galaxy 1,000,000 distances: 512240933238

run() ->
    Image = parse_image(input("day11.txt")),
    part1(Image),
    part2(Image).

part1(Image) ->
    Expanded = expand(Image, 1),

    Distances = galaxy_distances(Expanded),
    io:format("galaxy 2x distances: ~p~n", [lists:sum(Distances)]).

part2(Image) ->
    Expanded = expand(Image, 999999),

    Distances = galaxy_distances(Expanded),
    io:format("galaxy 1,000,000 distances: ~p~n", [lists:sum(Distances)]).

galaxy_distances(Image) ->
    [G | Galaxies] = maps:keys(Image),
    galaxy_distances(G, Galaxies, []).

galaxy_distances(_Galaxy, [], Distances) -> Distances;
galaxy_distances({max_x, max_y}, [NextG | Galaxies], Distances) ->
    galaxy_distances(NextG, Galaxies, Distances);
galaxy_distances(Galaxy, [NextG | Galaxies], Distances) ->
    Ds = [manhattan_distance(Galaxy, {X, Y})
          || {X, Y} <- [NextG | Galaxies],
             is_integer(X), is_integer(Y)
         ],
    galaxy_distances(NextG, Galaxies, Ds ++ Distances).

manhattan_distance({AX, AY}, {BX, BY}) ->
    abs(AX-BX) + abs(AY-BY).

expand(Image, Factor) ->
    expand(Image, Factor, maps:keys(Image)).

expand(#{{max_x, max_y} := {MaxX, MaxY}}=Image, Factor, Galaxies) ->
    {Xs, Ys} = lists:unzip(Galaxies),

    Image1 = expand_universe(Image, MaxY, tl(lists:reverse(lists:usort(Ys))), {0, Factor}),
    expand_universe(Image1, MaxX, tl(lists:reverse(lists:usort(Xs))), {Factor, 0}).

expand_universe(Image, 1, _Ns, _Adjustment) ->
    Image;
expand_universe(Image, N, [N | Ns], Adjustment) ->
    expand_universe(Image, N-1, Ns, Adjustment);
expand_universe(Image, N, Ns, Adjustment) ->
    Image1 = expand_galaxies(Image, N, Adjustment),
    expand_universe(Image1, N-1, Ns, Adjustment).

expand_galaxies(Image, N, Adjustment) ->
    {I, _N, _A} = maps:fold(fun shift_galaxy_down/3, {Image, N, Adjustment}, Image),
    I.

shift_galaxy_down({max_x, max_y}, {X, Y}, {Image, N, {DX, DY}=Adjustment}) ->
    {Image#{{max_x, max_y} => {X+DX, Y+DY}}, N, Adjustment};
shift_galaxy_down({GX, GY}, GNo, {Image, Y, {0, Factor}=Adjustment}) when GY > Y ->
    {maps:remove({GX, GY}, Image#{{GX, GY+Factor} => GNo}), Y, Adjustment};
shift_galaxy_down({GX, GY}, GNo, {Image, X, {Factor, 0}=Adjustment}) when GX > X ->
    {maps:remove({GX, GY}, Image#{{GX+Factor, GY} => GNo}), X, Adjustment};
shift_galaxy_down(_, _, {Image, Y, Adjustment}) ->
    {Image, Y, Adjustment}.

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.

parse_image(Input) ->
    Lines = binary:split(Input, <<$\n>>, ['global', 'trim']),
    parse_map(#{}, 1, 1, Lines).

parse_map(#{max_x := MaxX}=Map, MaxY, _G, []) ->
    maps:remove(max_x, Map#{{max_x, max_y} => {MaxX, MaxY-1}});
parse_map(Map, Y, G, [Line | Lines]) ->
    {Updated, _Y, G1, MaxX} = parse_row(Map, Y, G, Line),
    parse_map(Updated#{max_x => MaxX-1}, Y+1, G1, Lines).

parse_row(Map, Y, G, Line) ->
    lists:foldl(fun parse_cell/2
               ,{Map, Y, G, 1}
               ,[Cell || <<Cell>> <= Line]
               ).

parse_cell($., {Map, Y, G, X}) ->
    {Map, Y, G, X+1};
parse_cell($#, {Map, Y, G, X}) ->
    {Map#{{X, Y} => G}, Y, G+1, X+1}.
