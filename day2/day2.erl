-module(day2).

-export([sum_possible_games/1
        ,sum_power_of_minimum_cube_sets/1
        ]).

-define(TOTAL_CUBES, [{'red', 12}, {'green', 13}, {'blue', 14}]).

sum_possible_games(Input) ->
    Games = parse_games(Input),
    PossibleGames = lists:filter(fun is_possible_game/1, Games),
    lists:sum([GameId || {GameId, _} <- PossibleGames]).

sum_power_of_minimum_cube_sets(Input) ->
    Games = parse_games(Input),
    MinimumGameSets = [minimum_cube_set(Game) || Game <- Games],
    lists:sum([cube_set_power(MinimumGameSet) || MinimumGameSet <- MinimumGameSets]).

is_possible_game({_GameId, Subsets}) ->
    lists:all(fun is_possible_subset/1, Subsets).

is_possible_subset(Subset) ->
    lists:all(fun is_possible_color/1, Subset).

is_possible_color({Color, Count}) ->
    {Color, TotalCount} = lists:keyfind(Color, 1, ?TOTAL_CUBES),
    Count =< TotalCount.

minimum_cube_set({_GameId, Subsets}) ->
    CubeSet = lists:foldl(fun update_minimum_cube_set_from_subsets/2, #{}, Subsets),
    maps:to_list(CubeSet).

update_minimum_cube_set_from_subsets(Subset, Acc) ->
    lists:foldl(fun update_minimum_cube_set/2, Acc, Subset).

update_minimum_cube_set({Color, Count}, Acc) ->
    maps:update_with(Color, fun(MinCount) -> max(MinCount, Count) end, Count, Acc).

cube_set_power(CubeSet) ->
    %% Assuming power is 1 for empty cube set (though this does not happen in
    %% the input)
    lists:foldl(fun({_Color, Count}, Acc) -> Count * Acc end, 1, CubeSet).

parse_games(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    [parse_game(Line) || Line <- Lines].

parse_game(Line) ->
    {'match', [GameId, Subsets]} =
      re:run(Line, <<"Game (\\d+): (.*)">>, [{'capture', 'all_but_first', 'binary'}]),
    {binary_to_integer(GameId), parse_subsets(Subsets)}.

parse_subsets(SubsetsBin) ->
    Subsets = binary:split(SubsetsBin, <<";">>, ['global', 'trim_all']),
    [parse_subset(Subset) || Subset <- Subsets].

parse_subset(SubsetBin) ->
    Subset = binary:split(SubsetBin, <<",">>, ['global', 'trim_all']),
    [parse_color(Color) || Color <- Subset].

parse_color(ColorBin) ->
    {'match', [Count, Color]} =
      re:run(ColorBin, <<"(\\d+) (red|green|blue)">>, [{'capture', 'all_but_first', 'binary'}]),
    {color(Color), binary_to_integer(Count)}.

color(<<"red">>) -> 'red';
color(<<"green">>) -> 'green';
color(<<"blue">>) -> 'blue'.
