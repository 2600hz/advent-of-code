-module(main).
-export([start/0]). 

start() -> 
    {ok, Input} = file:read_file("input.txt"),
    L = convert_input(binary:split(Input, [<<"\n">>]), []),
    R = search_stars(L, L, []),
    io:format("~p~n", [R]).

search_stars(_, [], Results) -> Results;
search_stars([Val1|[]], [Val1|Remaining], Results) -> search_stars(Remaining, Remaining, Results);
search_stars([Val1|[Val2|Stars]], FullList, Results) when Val1 + Val2 =:= 2020 ->
    search_stars([Val1|Stars], FullList, [Val1 * Val2|Results]);
search_stars([Val1|[_|Stars]], FullList, Results) ->
    search_stars([Val1|Stars], FullList, Results).

convert_input([<<>>], FullList) -> FullList;
convert_input([Star|[Stars]], FullList) ->
    Val1 = binary_to_integer(Star),
    convert_input(binary:split(Stars, [<<"\n">>]), [Val1|FullList]).