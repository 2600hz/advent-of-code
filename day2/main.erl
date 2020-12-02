-module(main).
-export([start/0]).

start() -> 
    {ok, Input} = file:read_file("input.txt"),
    R = check_valid_passwords(binary:split(Input, [<<"\n">>]), 0),
    io:format("~p~n", [R]),
    ok.

check_valid_passwords([<<>>], Counter) -> Counter;
check_valid_passwords([Pw|[Pws]], Counter)->
    check_valid_passwords(binary:split(Pws, [<<"\n">>]), Counter + check(Pw));
check_valid_passwords([Pw], Counter) -> Counter + check(Pw).

check(Pw) ->
    Step1 = binary:replace(Pw, <<"-">>, <<",">>),
    Step2 = binary:replace(Step1, <<" ">>, <<",">>),
    Val1  = binary:replace(Step2, <<": ">>, <<",">>),
    [Min|[Max|[Letter|[Letters]]]] = binary:split(Val1, [<<",">>],[global]),
    MinValue = list_to_integer(binary_to_list(Min)),
    MaxValue = list_to_integer(binary_to_list(Max)),
    Count = length(binary:matches(Letters, Letter)),
    case (Count>=MinValue) and (Count=<MaxValue) of
        true ->
            1;

        false ->
            0
    end.