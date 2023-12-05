-module(day03).

-export([run/0]).

%% https://adventofcode.com/2023/day/3
%% parts number sum: 526404
%% gear ratio sum: 84399773

run() ->
    part1(input("day03.txt")),
    part2(input("day03.txt")).

part1(Input) ->
    Lines = binary:split(Input, <<$\n>>, ['global', 'trim']),
    Schematic = parse_lines(Lines),
    Parts = count_parts(Schematic),
    Sum = lists:sum([PartNo || {PartNo, _} <- Parts]),
    io:format("parts number sum: ~p~n", [Sum]).

count_parts(Schematic) ->
    count_parts(Schematic, [], 1, 1).

count_parts(Schematic, Parts, Row, Col) ->
    count_parts(Schematic, Parts, Row, Col
               ,maps:get({Row, Col}, Schematic, 'undefined')
               ).

count_parts(_Schematic, Parts, _Row, 1, 'undefined') ->
    Parts;
count_parts(Schematic, Parts, Row, _Col, 'undefined') ->
    count_parts(Schematic, Parts, Row+1, 1);
count_parts(Schematic, Parts, Row, Col, <<N>>)
  when N >= $0, N =< $9 ->
    maybe_count_part(Schematic, Parts, Row, Col+1, <<N>>);
count_parts(Schematic, Parts, Row, Col, _Other) ->
    count_parts(Schematic, Parts, Row, Col+1).

maybe_count_part(Schematic, Parts, Row, Col, N) ->
    maybe_count_part(Schematic, Parts, Row, Col, N
                    ,maps:get({Row, Col}, Schematic, 'undefined')
                    ).

maybe_count_part(Schematic, Parts, Row, Col, N, 'undefined') ->
    maybe_adjacent(Schematic, Parts, Row, Col, N);
maybe_count_part(Schematic, Parts, Row, Col, N, <<X>>)
  when X >= $0, X =< $9 ->
    maybe_count_part(Schematic, Parts, Row, Col+1, <<N/binary, X>>);
maybe_count_part(Schematic, Parts, Row, Col, N, <<$.>>) ->
    maybe_adjacent(Schematic, Parts, Row, Col, N);
maybe_count_part(Schematic, Parts, Row, Col, N, Special) ->
    count_part(Schematic, Parts, Row, Col, N, {Special, {Row, Col}}).

count_part(Schematic, Parts, Row, Col, N, Special) ->
    PartNo = binary_to_integer(N, 10),
    count_parts(Schematic, [{PartNo, Special} | Parts], Row, Col+1).

maybe_adjacent(Schematic, Parts, Row, Col, N) ->
    Len = byte_size(N),
    StartCol = Col-Len,

    ColsToCheck = lists:seq(StartCol-1, Col),

    check_adjacent(Schematic, Parts, Row, Col, N, ColsToCheck, [Row-1, Row, Row+1]).

check_adjacent(Schematic, Parts, Row, Col, _N, _ColsToCheck, []) ->
    count_parts(Schematic, Parts, Row, Col);
check_adjacent(Schematic, Parts, Row, Col, N, ColsToCheck, [CheckRow | CheckRows]) ->
    case check_adjacent_row(Schematic, ColsToCheck, CheckRow) of
        {'true', Special} -> count_part(Schematic, Parts, Row, Col, N, Special);
        'false' -> check_adjacent(Schematic, Parts, Row, Col, N, ColsToCheck, CheckRows)
    end.

check_adjacent_row(_Schematic, [], _Row) -> 'false';
check_adjacent_row(Schematic, [Col | ColsToCheck], Row) ->
    case maps:get({Row, Col}, Schematic, 'undefined') of
        'undefined' -> check_adjacent_row(Schematic, ColsToCheck, Row);
        <<$.>> -> check_adjacent_row(Schematic, ColsToCheck, Row);
        <<N:8>> ->
            case N < $0 orelse N > $9 of
                'false' -> check_adjacent_row(Schematic, ColsToCheck, Row);
                'true' ->
                    {'true', {<<N>>, {Row, Col}}}
            end
    end.

parse_lines(Lines) ->
    {_Row, Schematic} = lists:foldl(fun parse_line/2, {1, #{}}, Lines),
    Schematic.

parse_line(Line, {Row, Schematic}) ->
    Len = byte_size(Line),
    LineSchema = [<<Y>> || <<Y>> <= Line],
    Cols = lists:zip(lists:seq(1, Len), LineSchema),
    {Row+1
    ,lists:foldl(fun({Col, Char}, M) -> M#{{Row, Col} => Char} end
                ,Schematic
                ,Cols
                )
    }.

part2(Input) ->
    Lines = binary:split(Input, <<$\n>>, ['global', 'trim']),
    Schematic = parse_lines(Lines),
    Parts = count_parts(Schematic),

    Ratios = count_ratios(lists:keysort(2, Parts), 0),

    io:format("gear ratio sum: ~p~n", [Ratios]).

count_ratios(Parts, Sum) ->
    count_ratios(Parts, Sum, []).

count_ratios([], Sum, _) -> Sum;
count_ratios([{_PartNo, {<<$*>>, _Loc}}=Gear | Parts], Sum, []) ->
    count_ratios(Parts, Sum, [Gear]);
count_ratios([{_PartNoTwo, {<<$*>>, Loc}}=Gear | Parts]
            ,Sum
            ,[{_PartNoOne, {<<$*>>, Loc}}|_]=Gears
            ) ->
    count_ratios(Parts, Sum, [Gear | Gears]);
count_ratios([{_PartNo, {<<$*>>, Loc}}=Gear | Parts]
            ,Sum
            ,[{PartNoOne, {<<$*>>, OtherLoc}}
             ,{PartNoTwo, {<<$*>>, OtherLoc}}
             ]
            ) when Loc =/= OtherLoc ->
    count_ratios(Parts, Sum + (PartNoOne * PartNoTwo), [Gear]);
count_ratios([{_PartNo, {<<$*>>, Loc}}=NewGear | Parts]
            ,Sum
            ,[{_PartNoOne, {<<$*>>, OtherLoc}} | _]
            ) when Loc =/= OtherLoc ->
    count_ratios(Parts, Sum, [NewGear]);
count_ratios([_Part | Parts], Sum, [{PartNoOne, _}, {PartNoTwo, _}]) ->
    count_ratios(Parts, Sum + (PartNoOne * PartNoTwo), []);
count_ratios([_Part | Parts], Sum, _Gear) ->
    count_ratios(Parts, Sum, []).

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.
