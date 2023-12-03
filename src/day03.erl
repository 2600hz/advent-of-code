-module(day03).

-export([run/0]).

%% --- Day 3: Gear Ratios ---

%% You and the Elf eventually reach a gondola lift station; he says
%% the gondola lift will take you up to the water source, but this is
%% as far as he can bring you. You go inside.

%% It doesn't take long to find the gondolas, but there seems to be a
%% problem: they're not moving.

%% "Aaah!"

%% You turn around to see a slightly-greasy Elf with a wrench and a
%% look of surprise. "Sorry, I wasn't expecting anyone! The gondola
%% lift isn't working right now; it'll still be a while before I can
%% fix it." You offer to help.

%% The engineer explains that an engine part seems to be missing from
%% the engine, but nobody can figure out which one. If you can add up
%% all the part numbers in the engine schematic, it should be easy to
%% work out which part is missing.

%% The engine schematic (your puzzle input) consists of a visual
%% representation of the engine. There are lots of numbers and symbols
%% you don't really understand, but apparently any number adjacent to
%% a symbol, even diagonally, is a "part number" and should be
%% included in your sum. (Periods (.) do not count as a symbol.)

%% Here is an example engine schematic:

%% 467..114..
%% ...*......
%% ..35..633.
%% ......#...
%% 617*......
%% .....+.58.
%% ..592.....
%% ......755.
%% ...$.*....
%% .664.598..

%% In this schematic, two numbers are not part numbers because they
%% are not adjacent to a symbol: 114 (top right) and 58 (middle
%% right). Every other number is adjacent to a symbol and so is a part
%% number; their sum is 4361.

%% Of course, the actual engine schematic is much larger. What is the
%% sum of all of the part numbers in the engine schematic?

%% --- Part Two ---

%% The engineer finds the missing part and installs it in the engine!
%% As the engine springs to life, you jump in the closest gondola,
%% finally ready to ascend to the water source.

%% You don't seem to be going very fast, though. Maybe something is
%% still wrong? Fortunately, the gondola has a phone labeled "help",
%% so you pick it up and the engineer answers.

%% Before you can explain the situation, she suggests that you look
%% out the window. There stands the engineer, holding a phone in one
%% hand and waving with the other. You're going so slowly that you
%% haven't even left the station. You exit the gondola.

%% The missing part wasn't the only issue - one of the gears in the
%% engine is wrong. A gear is any * symbol that is adjacent to exactly
%% two part numbers. Its gear ratio is the result of multiplying those
%% two numbers together.

%% This time, you need to find the gear ratio of every gear and add
%% them all up so that the engineer can figure out which gear needs to
%% be replaced.

%% Consider the same engine schematic again:

%% 467..114..
%% ...*......
%% ..35..633.
%% ......#...
%% 617*......
%% .....+.58.
%% ..592.....
%% ......755.
%% ...$.*....
%% .664.598..

%% In this schematic, there are two gears. The first is in the top
%% left; it has part numbers 467 and 35, so its gear ratio is
%% 16345. The second gear is in the lower right; its gear ratio is
%% 451490. (The * adjacent to 617 is not a gear because it is only
%% adjacent to one part number.) Adding up all of the gear ratios
%% produces 467835.

%% What is the sum of all of the gear ratios in your engine schematic?

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
