#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 4: Giant Squid ---

%% You're already almost 1.5km (almost a mile) below the surface of
%% the ocean, already so deep that you can't see any sunlight. What
%% you can see, however, is a giant squid that has attached itself to
%% the outside of your submarine.

%% Maybe it wants to play bingo?

%% Bingo is played on a set of boards each consisting of a 5x5 grid of
%% numbers. Numbers are chosen at random, and the chosen number is
%% marked on all boards on which it appears. (Numbers may not appear
%% on all boards.) If all numbers in any row or any column of a board
%% are marked, that board wins. (Diagonals don't count.)

%% The submarine has a bingo subsystem to help passengers (currently,
%% you and the giant squid) pass the time. It automatically generates
%% a random order in which to draw numbers and a random set of boards
%% (your puzzle input). For example:

%% 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

%% 22 13 17 11  0
%%  8  2 23  4 24
%% 21  9 14 16  7
%%  6 10  3 18  5
%%  1 12 20 15 19

%%  3 15  0  2 22
%%  9 18 13 17  5
%% 19  8  7 25 23
%% 20 11 10 24  4
%% 14 21 16 12  6

%% 14 21 17 24  4
%% 10 16 15  9 19
%% 18  8 23 26 20
%% 22 11 13  6  5
%%  2  0 12  3  7

%% After the first five numbers are drawn (7, 4, 9, 5, and 11), there
%% are no winners, but the boards are marked as follows (shown here
%% adjacent to each other to save space):

%% 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
%%  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
%% 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
%%  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
%%  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

%% After the next six numbers are drawn (17, 23, 2, 0, 14, and 21),
%% there are still no winners:

%% 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
%%  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
%% 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
%%  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
%%  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

%% Finally, 24 is drawn:

%% 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
%%  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
%% 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
%%  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
%%  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

%% At this point, the third board wins because it has at least one
%% complete row or column of marked numbers (in this case, the entire
%% top row is marked: 14 21 17 24 4).

%% The score of the winning board can now be calculated. Start by
%% finding the sum of all unmarked numbers on that board; in this
%% case, the sum is 188. Then, multiply that sum by the number that
%% was just called when the board won, 24, to get the final score, 188
%% * 24 = 4512.

%% To guarantee victory against the giant squid, figure out which
%% board will win first. What will your final score be if you choose
%% that board?

main(_) ->
    Input = read_input("p4.txt"),
    p4_1(Input),
    p4_2(Input).

p4_1({BingoNumbers, Boards}) ->
    {N, {{Type, _Data}, Board}} = find_winner(Boards, BingoNumbers),
    Score = score_board(Board),
    io:format("winning ~s board at ~p~nscore: ~p final: ~p~n"
             ,[Type, N, Score, Score * N]
             ).

score_board(Rows) ->
    lists:sum([Cell || Row <- Rows,
                       Cell <- Row,
                       is_number(Cell)
              ]).

find_winner(Boards, [N | BingoNumbers]) ->
    {N, NewBoards} = lists:foldl(fun mark_board/2, {N, []}, Boards),
    case lists:foldl(fun is_winning_board/2, 'undefined', NewBoards) of
        'undefined' -> find_winner(NewBoards, BingoNumbers);
        Winner -> {N, Winner}
    end.

mark_board(Board, {N, NewBoards}) ->
    {N, NewBoard} = lists:foldr(fun mark_row/2, {N, []}, Board),
    {N, [NewBoard | NewBoards]}.

mark_row(Row, {N, Board}) ->
    {N, NewRow} = lists:foldr(fun mark_cell/2, {N, []}, Row),
    {N, [NewRow | Board]}.

mark_cell(N, {N, Row}) ->
    {N, [{N, 'true'} | Row]};
mark_cell(C, {N, Row}) ->
    {N, [C | Row]}.

is_winning_board(Rows, Acc) ->
    case is_winning_row(Rows) of
        'false' -> is_winning_column(Rows, Acc);
        Winner -> {Winner, Rows}
    end.

is_winning_row([]) -> 'false';
is_winning_row([Row | Rows]) ->
    case lists:all(fun is_winning_cell/1, Row) of
        'true' -> {'row', Row};
        'false' -> is_winning_row(Rows)
    end.

is_winning_column([FirstRow | Rows], Acc) ->
    case is_winning_column(Rows, FirstRow, 1) of
        'false' -> Acc;
        Winner -> {Winner, [FirstRow | Rows]}
    end.

is_winning_column(Rows, [{_, 'true'} | Cells], Column) ->
    case lists:all(fun is_winning_cell/1
                  ,[lists:nth(Column, Row) || Row <- Rows]
                  )
    of
        'true' -> {'column', Column};
        'false' ->
            is_winning_column(Rows, Cells, Column+1)
    end;
is_winning_column(Rows, [_ | Cells], Column) ->
    is_winning_column(Rows, Cells, Column+1);
is_winning_column(_Rows, [], _Column) -> 'false'.


is_winning_cell({_, 'true'}) -> 'true';
is_winning_cell(_) -> 'false'.

p4_2(Input) ->
    Input.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [BingoNumbers, <<>> | Boards] = binary:split(Lines, <<"\n">>, ['global']),
    {[binary_to_integer(N, 10) || N <- binary:split(BingoNumbers, <<",">>, ['global'])]
    ,parse_boards(Boards)
    }.

parse_boards(Boards) ->
    tl(lists:foldl(fun parse_board/2, [[]], Boards)).

parse_board(<<>>, [Board | Boards]) ->
    [[] | [lists:reverse(Board) | Boards]];
parse_board(Row, [Board | Boards]) ->
    RowNs = [binary_to_integer(N, 10) || N <- binary:split(Row, <<" ">>, ['global']),
                                         N =/= <<>>
            ],
    [[RowNs | Board] | Boards].
