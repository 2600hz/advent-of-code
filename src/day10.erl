-module(day10).

-export([run/0]).

%% https://adventofcode.com/2023/day/10/
%% furthest pipe is 6838 steps

run() ->
    Map = parse_map(input("day10_part1.txt")),
    part1(Map),
    part2(Map).

part1(Map) ->
    Furthest = find_furthest(Map),
    io:format("furthest pipe is ~p steps~n", [Furthest]).

find_furthest(#{$S := {X, Y}}=Map) ->
    find_furthest(Map, {X,Y}).

find_furthest(Map, {X, Y}) ->
    Candidates = [{{X, Y-1}, 's'} % above S must have a 's' attribute
                 ,{{X, Y+1}, 'n'} % below S must have a 'n' attribute
                 ,{{X-1, Y}, 'e'} % left of S must have a 'e' attribute
                 ,{{X+1, Y}, 'w'} % right of S must have a 'w' attribute
                 ],
    [Next1, Next2] = [{NextXY, Dir} || {NextXY, Dir} <- Candidates,
                                       has_attribute(Map, NextXY, Dir)
                     ],
    %% io:format(" ~p => [~p ~p]~n", [{X, Y}, Next1, Next2]),
    find_furthest(Map, {Next1, Next2}, 1).

find_furthest(#{$S := {X, Y}}, {{{X, Y}, _}, _}, Steps) ->
    io:format("looped after ~p steps~n", [Steps]),
    Steps div 2;
find_furthest(#{$S := {X, Y}}, {_, {{X, Y}, _}}, Steps) ->
    io:format("looped after ~p steps~n", [Steps]),
    Steps div 2;
find_furthest(_Map, {{{X, Y}, _}, {{X, Y}, _}}, Steps) ->
    Steps; %% arrived half way around
find_furthest(Map, {Cell1, Cell2}, Steps) ->
    %% io:format("  ~p: ~p =>", [Steps, Cell1]),
    Next1 = step(Map, Cell1),
    %% io:format(" ~p ~p =>", [Next1, Cell2]),
    Next2 = step(Map, Cell2),
    %% io:format(" ~p~n", [Next2]),
    find_furthest(Map, {Next1, Next2}, Steps + 1).

step(Map, {XY, Dir}) ->
    Dirs = maps:get(XY, Map),
    [NextDir] = lists:delete(Dir, Dirs),
    move(XY, NextDir).

move({X, Y}, 'n') ->
    {{X, Y-1}, 's'};
move({X, Y}, 's') ->
    {{X, Y+1}, 'n'};
move({X, Y}, 'e') ->
    {{X+1, Y}, 'w'};
move({X, Y}, 'w') ->
    {{X-1, Y}, 'e'}.

has_attribute(Map, NextXY, Dir) ->
    lists:member(Dir, maps:get(NextXY, Map, [])).

part2(Map) ->
    Map.

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.

parse_map(Input) ->
    Lines = binary:split(Input, <<$\n>>, ['global', 'trim']),
    parse_map(#{}, 1, Lines).

parse_map(Map, _Y, []) -> Map;
parse_map(Map, Y, [Line | Lines]) ->
    {Updated, _, _} = parse_row(Map, Y, Line),
    parse_map(Updated, Y+1, Lines).

parse_row(Map, Y, Line) ->
    lists:foldl(fun parse_cell/2
               ,{Map, Y, 1}
               ,[Cell || <<Cell>> <= Line]
               ).

parse_cell($|, {Map, Y, X}) ->
    {Map#{{X, Y} => ['n', 's']}, Y, X+1};
parse_cell($-, {Map, Y, X}) ->
    {Map#{{X, Y} => ['e', 'w']}, Y, X+1};
parse_cell($L, {Map, Y, X}) ->
    {Map#{{X, Y} => ['n', 'e']}, Y, X+1};
parse_cell($J, {Map, Y, X}) ->
    {Map#{{X, Y} => ['n', 'w']}, Y, X+1};
parse_cell($7, {Map, Y, X}) ->
    {Map#{{X, Y} => ['s', 'w']}, Y, X+1};
parse_cell($F, {Map, Y, X}) ->
    {Map#{{X, Y} => ['s', 'e']}, Y, X+1};
parse_cell($., {Map, Y, X}) ->
    {Map#{{X, Y} => []}, Y, X+1};
parse_cell($S, {Map, Y, X}) ->
    {Map#{{X, Y} => [], $S => {X, Y}}, Y, X+1}.
