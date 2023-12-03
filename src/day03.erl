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

%% parts number sum: 526404

run() ->
    part1(input("day03.txt")),
    part2(input("day03.txt")).

part1(Input) ->
    Lines = binary:split(Input, <<$\n>>, ['global', 'trim']),
    Schematic = parse_lines(Lines),
    Sum = count_parts(Schematic),
    io:format("parts number sum: ~p~n", [Sum]).

count_parts(Schematic) ->
    count_parts(Schematic, 0, 1, 1).

count_parts(Schematic, Sum, Row, Col) ->
    count_parts(Schematic, Sum, Row, Col
               ,maps:get({Row, Col}, Schematic, 'undefined')
               ).

count_parts(_Schematic, Sum, _Row, 1, 'undefined') ->
    %% io:format("finished~n"),
    Sum;
count_parts(Schematic, Sum, Row, _Col, 'undefined') ->
    %% io:format("starting new row ~p~n", [Row+1]),
    count_parts(Schematic, Sum, Row+1, 1);
count_parts(Schematic, Sum, Row, Col, <<N>>)
  when N >= $0, N =< $9 ->
    %% io:format("detected number at ~p,~p: ~p~n", [Row, Col, <<N>>]),
    maybe_count_part(Schematic, Sum, Row, Col+1, <<N>>);
count_parts(Schematic, Sum, Row, Col, _Other) ->
    count_parts(Schematic, Sum, Row, Col+1).

maybe_count_part(Schematic, Sum, Row, Col, N) ->
    maybe_count_part(Schematic, Sum, Row, Col, N
                    ,maps:get({Row, Col}, Schematic, 'undefined')
                    ).

maybe_count_part(Schematic, Sum, Row, Col, N, 'undefined') ->
    maybe_adjacent(Schematic, Sum, Row, Col, N);
maybe_count_part(Schematic, Sum, Row, Col, N, <<X>>)
  when X >= $0, X =< $9 ->
    maybe_count_part(Schematic, Sum, Row, Col+1, <<N/binary, X>>);
maybe_count_part(Schematic, Sum, Row, Col, N, <<$.>>) ->
    maybe_adjacent(Schematic, Sum, Row, Col, N);
maybe_count_part(Schematic, Sum, Row, Col, N, _Special) ->
    %% io:format("found symbol at ~p,~p: ~p~n", [Row, Col, _Special]),
    count_part(Schematic, Sum, Row, Col, N).

count_part(Schematic, Sum, Row, Col, N) ->
    PartNo = binary_to_integer(N, 10),
    %% io:format("found part at ~p,~p: ~p~n", [Row, Col, PartNo]),
    count_parts(Schematic, Sum + PartNo, Row, Col+1).

maybe_adjacent(Schematic, Sum, Row, Col, N) ->
    %% io:format("checking for adjacent at ~p,~p: ~p~n", [Row, Col, N]),
    Len = byte_size(N),
    StartCol = Col-Len,

    ColsToCheck = lists:seq(StartCol-1, Col),

    check_adjacent(Schematic, Sum, Row, Col, N, ColsToCheck, [Row-1, Row, Row+1]).

check_adjacent(Schematic, Sum, Row, Col, _N, _ColsToCheck, []) ->
    %% io:format("not a part number at ~p,~p: ~p~n", [Row, Col, _N]),
    count_parts(Schematic, Sum, Row, Col);
check_adjacent(Schematic, Sum, Row, Col, N, ColsToCheck, [CheckRow | CheckRows]) ->
    case check_adjacent_row(Schematic, ColsToCheck, CheckRow) of
        'true' -> count_part(Schematic, Sum, Row, Col, N);
        'false' -> check_adjacent(Schematic, Sum, Row, Col, N, ColsToCheck, CheckRows)
    end.

check_adjacent_row(Schematic, ColsToCheck, CheckRow) ->
    lists:any(fun(Col) -> check_adjacent_column(Schematic, CheckRow, Col) end
             ,ColsToCheck
             ).

check_adjacent_column(Schematic, Row, Col) ->
    case maps:get({Row, Col}, Schematic, 'undefined') of
        'undefined' -> 'false';
        <<$.>> -> 'false';
        <<N:8>> ->
            case N < $0 orelse N > $9 of
                'false' -> 'false';
                'true' ->
                    %% io:format("found symbol at ~p,~p: ~p~n", [Row, Col, <<N>>]),
                    'true'
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
    Input.

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.
