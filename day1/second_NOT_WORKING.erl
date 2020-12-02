-module(second).
-export([start/0]). 

start() -> 
    {ok, Input} = file:read_file("input.txt"),
    L = lists:reverse(convert_input(binary:split(Input, [<<"\n">>]), [])),
    %L = [1721,979,366,299,675,1456],
    [First|Tail] = L,
    R = search_stars(First, Tail, L, []),
    io:format("~p~n", [R]).

search_stars(_, [Val1|[]], [Val1|[]], Results) -> Results;
search_stars(First, [Val1|[]], [First|Remaining], Results) -> 
    search_stars(Val1, Remaining, Remaining, Results);
search_stars(_, [Val1|[]], [Val1|Remaining], Results) ->
    search_stars(Val1, Remaining, Remaining, Results);
search_stars(First, [Val1|[Val2|Stars]], FullList, Results) when Val1 + Val2 + First =:= 2020 ->
    search_stars(First, [Val1|Stars], FullList, [Val1 * Val2 * First|Results]);
search_stars(First, [Val1|[_|Stars]], FullList, Results) ->
    search_stars(First, [Val1|Stars], FullList, Results).

convert_input([<<>>], FullList) -> FullList;
convert_input([Star|[Stars]], FullList) ->
    Val1 = binary_to_integer(Star),
    convert_input(binary:split(Stars, [<<"\n">>]), [Val1|FullList]).