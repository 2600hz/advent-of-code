#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 10: Syntax Scoring ---

%% You ask the submarine to determine the best route out of the
%% deep-sea cave, but it only replies:

%% Syntax error in navigation subsystem on line: all of them

%% All of them?! The damage is worse than you thought. You bring up a
%% copy of the navigation subsystem (your puzzle input).

%% The navigation subsystem syntax is made of several lines containing
%% chunks. There are one or more chunks on each line, and chunks
%% contain zero or more other chunks. Adjacent chunks are not
%% separated by any delimiter; if one chunk stops, the next chunk (if
%% any) can immediately start. Every chunk must open and close with
%% one of four legal pairs of matching characters:

%%     If a chunk opens with (, it must close with ).
%%     If a chunk opens with [, it must close with ].
%%     If a chunk opens with {, it must close with }.
%%     If a chunk opens with <, it must close with >.

%% So, () is a legal chunk that contains no other chunks, as is
%% []. More complex but valid chunks include ([]), {()()()}, <([{}])>,
%% [<>({}){}[([])<>]], and even (((((((((()))))))))).

%% Some lines are incomplete, but others are corrupted. Find and
%% discard the corrupted lines first.

%% A corrupted line is one where a chunk closes with the wrong
%% character - that is, where the characters it opens and closes with
%% do not form one of the four legal pairs listed above.

%% Examples of corrupted chunks include (], {()()()>, (((()))}, and
%% <([]){()}[{}]). Such a chunk can appear anywhere within a line, and
%% its presence causes the whole line to be considered corrupted.

%% For example, consider the following navigation subsystem:

%% [({(<(())[]>[[{[]{<()<>>
%% [(()[<>])]({[<{<<[]>>(
%% {([(<{}[<>[]}>{[]{[(<()>
%% (((({<>}<{<{<>}{[]{[]{}
%% [[<[([]))<([[{}[[()]]]
%% [{[{({}]{}}([{[{{{}}([]
%% {<[[]]>}<{[{[{[]{()[[[]
%% [<(<(<(<{}))><([]([]()
%% <{([([[(<>()){}]>(<<{{
%% <{([{{}}[<[[[<>{}]]]>[]]

%% Some of the lines aren't corrupted, just incomplete; you can ignore
%% these lines for now. The remaining five lines are corrupted:

%%     {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
%%     [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
%%     [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
%%     [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
%%     <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.

%% Stop at the first incorrect closing character on each corrupted
%% line.

%% Did you know that syntax checkers actually have contests to see who
%% can get the high score for syntax errors in a file? It's true! To
%% calculate the syntax error score for a line, take the first illegal
%% character on the line and look it up in the following table:

%%     ): 3 points.
%%     ]: 57 points.
%%     }: 1197 points.
%%     >: 25137 points.

%% In the above example, an illegal ) was found twice (2*3 = 6
%% points), an illegal ] was found once (57 points), an illegal } was
%% found once (1197 points), and an illegal > was found once (25137
%% points). So, the total syntax error score for this file is
%% 6+57+1197+25137 = 26397 points!

%% Find the first illegal character in each corrupted line of the
%% navigation subsystem. What is the total syntax error score for
%% those errors?

main(_) ->
    Input = read_input("p10.txt"),
    p10_1(Input),
    p10_2(Input).

p10_1(Input) ->
    Illegal = lists:foldl(fun find_corrupted/2, [], Input),
    Score = lists:sum([score_illegal(I) || I <- Illegal]),
    io:format("illegal: ~p~n", [Score]).

score_illegal($)) -> 3;
score_illegal($]) -> 57;
score_illegal($}) -> 1197;
score_illegal($>) -> 25137.

find_corrupted(Chunks, IllegalChars) ->
    case parse_chunks(Chunks) of
        'complete' -> IllegalChars;
        'incomplete' -> IllegalChars;
        {'illegal', IC} -> [IC | IllegalChars]
    end.

parse_chunks(Chunks) ->
    parse_chunks(Chunks, []).

parse_chunks(<<>>, []) -> 'complete';
parse_chunks(<<>>, _Stack) -> 'incomplete';
parse_chunks(<<Open:8, Rest/binary>>, Stack)
  when Open =:= $[;
       Open =:= ${;
       Open =:= $(;
       Open =:= $<
       ->
    parse_chunks(Rest, [Open | Stack]);
parse_chunks(<<Close:8, Rest/binary>>, [Top | Stack]) ->
    case closes(Top, Close) of
        'true' -> parse_chunks(Rest, Stack);
        'false' -> {'illegal', Close}
    end.

closes(${, $}) -> 'true';
closes($[, $]) -> 'true';
closes($(, $)) -> 'true';
closes($<, $>) -> 'true';
closes(_, _) -> 'false'.

p10_2(Input) ->
    Input.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [Line || Line <- binary:split(Lines, <<"\n">>, ['global']), Line =/= <<>>].
