-module(day1_2).
-export([main/1]).

-spec main(list()) -> integer().
main(Path) ->
    Input = get_input(Path),
    max_calories(Input, 0, []).

max_calories([], Acc, Max) ->
    Calories = lists:reverse(lists:sort([Acc|Max])),
    [First|[Second|[Third|_]]]=Calories,
    First+Second+Third;
max_calories([<<>>|Calories], Acc, Max) -> max_calories(Calories, 0, [Acc|Max]);
max_calories([Calorie|Calories], Acc, Max) ->
    Cal = erlang:list_to_integer(erlang:binary_to_list(Calorie)),
    max_calories(Calories, Acc + Cal, Max).

get_input(Path) ->
    {ok, Txt} = file:read_file(Path),
    binary:split(Txt, <<"\n">>, [global]).