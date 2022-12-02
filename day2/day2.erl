-module(day2).
-export([main/1]).

-spec main(list()) -> {integer(), integer()}.
main(Path) ->
    Input = get_input(Path),
    rps(Input, {0, 0}).

rps([], {P1, P2}) -> {P1,P2};
rps([Binary|List], {P1, P2}) ->
    case binary:split(Binary, <<" ">>, [global]) of
        [<<"A">>, <<"Y">>] -> rps(List, {P1+1, P2+8});
        [<<"B">>, <<"X">>] -> rps(List, {P1+8, P2+1});
        [<<"C">>, <<"Z">>] -> rps(List, {P1+6, P2+6});

        [<<"A">>, <<"X">>] -> rps(List, {P1+4, P2+4});
        [<<"B">>, <<"Y">>] -> rps(List, {P1+5,P2+5});

        [<<"C">>, <<"Y">>] -> rps(List, {P1+9,P2+2});
        [<<"B">>, <<"Z">>] -> rps(List, {P1+2,P2+9});

        [<<"A">>, <<"Z">>] -> rps(List, {P1+7, P2+3});
        [<<"C">>, <<"X">>] -> rps(List, {P1+3,P2+7});
        [<<>>] -> {P1, P2}
    end.

get_input(Path) ->
    {ok, Txt} = file:read_file(Path),
    binary:split(Txt, <<"\n">>, [global]).