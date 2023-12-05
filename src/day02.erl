-module(day02).

-export([run/0]).

%% https://adventofcode.com/2023/day/2
%% Part 1: Sum of Game IDs: 3035
%% Part 2: Sum of power of sets: 66027

run() ->
    part1(input("day02.txt")),
    part2(input("day02.txt")).

part1(Input) ->
    GameBins = binary:split(Input, <<$\n>>, ['global', 'trim']),
    Games = lists:reverse(lists:foldl(fun read_game/2, [], GameBins)),
    BagContents = #{<<"red">> => 12
                   ,<<"green">> => 13
                   ,<<"blue">> => 14
                   },
    {Sum, _} = lists:foldl(fun games_matching_bag/2, {0, BagContents}, Games),
    io:format("Sum of Game IDs: ~p~n", [Sum]).

part2(Input) ->
    GameBins = binary:split(Input, <<$\n>>, ['global', 'trim']),
    Games = lists:reverse(lists:foldl(fun read_game/2, [], GameBins)),

    SumPower = fewest_cubes(Games),
    io:format("Sum of power of sets: ~p~n", [SumPower]).

games_matching_bag({GameNo, Selections}, {Sum, BagContents}) ->
    case lists:all(fun(Selection) -> selection_matches_bag(Selection, BagContents) end
                  ,Selections
                  )
    of
        'true' ->
            {Sum + GameNo, BagContents};
        'false' ->
            {Sum, BagContents}
    end.

selection_matches_bag(Selection, BagContents) ->
    lists:all(fun({Color, Cubes}) ->
                      maps:get(Color, BagContents) >= Cubes
              end
             ,Selection
             ).

fewest_cubes(Games) ->
    lists:foldl(fun fewest_cubes_in_game/2, 0, Games).

fewest_cubes_in_game({_Game, Selections}, SumPower) ->
    fewest_cubes_in_game(Selections) + SumPower.

fewest_cubes_in_game(Selections) ->
    {R, G, B} = lists:foldl(fun fewest_cubes_in_selection/2, {0, 0, 0}, Selections),
    R * G * B.

fewest_cubes_in_selection(Selection, {R, G, B}) ->
    {max(proplists:get_value(<<"red">>, Selection, 0), R)
    ,max(proplists:get_value(<<"green">>, Selection, 0), G)
    ,max(proplists:get_value(<<"blue">>, Selection, 0), B)
    }.

read_game(Line, Games) ->
    [<<"Game ", N/binary>>, Rest] = binary:split(Line, <<": ">>),
    SelectionBins = binary:split(Rest, <<"; ">>, ['global', 'trim']),
    Selections = lists:foldl(fun read_selection/2, [], SelectionBins),
    [{binary_to_integer(N, 10), lists:reverse(Selections)} | Games].

read_selection(CubesBin, Selections) ->
    Cubes = binary:split(CubesBin, <<", ">>, ['global', 'trim']),
    [[read_cube(Cube) || Cube <- Cubes]
    | Selections
    ].

read_cube(Cube) ->
    [N, Color] = binary:split(Cube, <<" ">>),
    {Color, binary_to_integer(N, 10)}.


input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.
