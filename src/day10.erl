-module(day10).

-export([run/0]).

%% https://adventofcode.com/2023/day/10/
%% furthest pipe is 6838 steps
%% area inside loop: 451

run() ->
    Map1 = parse_map(input("day10.txt")),
    part1(Map1),
    part2(Map1).

part1(Map) ->
    WalkedMap = walk_loop(Map),
    Steps = maps:fold(fun(_Point, {_Dirs, 'loop'}, S) -> S+1; (_, _, S) -> S end, 1, WalkedMap),
    io:format("furthest pipe is ~p steps~n", [Steps div 2]).

walk_loop(#{$S := {X, Y}}=Map) ->
    Candidates = [{{X, Y-1}, 's'} % above S must have a 's' attribute
                 ,{{X, Y+1}, 'n'} % below S must have a 'n' attribute
                 ,{{X-1, Y}, 'e'} % left of S must have a 'e' attribute
                 ,{{X+1, Y}, 'w'} % right of S must have a 'w' attribute
                 ],
    [{N1, D1}=Next1, {N2, D2}=Next2] = [{NextXY, Dir} || {NextXY, Dir} <- Candidates,
                                                         has_attribute(Map, NextXY, Dir)
                                       ],
    %% io:format(" ~p => [~p ~p]~n", [{X, Y}, Next1, Next2]),
    walk_loop(Map#{{X, Y} => {[not_dir(D1), not_dir(D2)], 'loop'}
                  ,N1 => {maps:get(N1, Map), 'loop'}
                  ,N2 => {maps:get(N2, Map), 'loop'}
                  }
             ,{Next1, Next2}
             ).

walk_loop(Map, {{{X, Y}, _}, {{X, Y}, _}}) ->
    Map; %% arrived half way around
walk_loop(Map, {Cell1, Cell2}) ->
    %% io:format("  ~p: ~p =>", [Steps, Cell1]),
    {N1, _} = Next1 = step(Map, Cell1),
    %% io:format(" ~p ~p =>", [Next1, Cell2]),
    {N2, _} = Next2 = step(Map, Cell2),
    %% io:format(" ~p~n", [Next2]),
    walk_loop(Map#{N1 => {maps:get(N1, Map), 'loop'}
                  ,N2 => {maps:get(N2, Map), 'loop'}
                  }
             ,{Next1, Next2}
             ).

step(Map, {XY, Dir}) ->
    Dirs = case maps:get(XY, Map) of
               {Ds, 'loop'} -> Ds;
               Ds -> Ds
           end,
    [NextDir] = lists:delete(Dir, Dirs),
    move(XY, NextDir).

not_dir('n') -> 's';
not_dir('s') -> 'n';
not_dir('e') -> 'w';
not_dir('w') -> 'e'.

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
    InnerPoints = calc_area(Map),
    io:format("area inside loop: ~p~n", [length(InnerPoints)]).

calc_area(Map) ->
    WalkedMap = walk_loop(Map),
    calc_inner(WalkedMap).

calc_inner(Map) ->
    calc_inner(Map, {{1, 1}, 'out'}, []).

calc_inner(Map, {{X, Y}, LoopDir}, InnerPoints) ->
    case maps:get({X, Y}, Map, 'undefined') of
        'undefined' when X =:= 1 ->
            %% end of map
            InnerPoints;
        'undefined' ->
            %% reached far east of map, \r\n
            calc_inner(Map, {{1, Y+1}, 'out'}, InnerPoints);
        [] when LoopDir =:= 'out' ->
            %% outer point
            calc_inner(Map, {{X+1, Y}, LoopDir}, InnerPoints);
        [] when LoopDir =:= 'in' ->
            %% inner point
            calc_inner(Map, {{X+1, Y}, LoopDir}, [{X, Y} | InnerPoints]);
        {Dirs, 'loop'} ->
            case lists:sort(Dirs) of
                ['n', 's'] ->
                    %% loop pipe going N/S
                    calc_inner(Map, {{X+1, Y}, not_loop_dir(LoopDir)}, InnerPoints);
                ['e', 'w'] ->
                    %% loop pipe going E/W
                    calc_inner(Map, {{X+1, Y}, LoopDir}, InnerPoints);
                ['e', 's'] ->
                    %% loop pipe goes east and bends S
                    calc_inner(Map, {{X+1, Y}, not_loop_dir(LoopDir)}, InnerPoints);
                ['e', 'n'] ->
                    %% loop pipe goes east and bends N
                    calc_inner(Map, {{X+1, Y}, LoopDir}, InnerPoints);
                ['s', 'w'] ->
                    %% loop pipe goes west and bends S
                    calc_inner(Map, {{X+1, Y}, not_loop_dir(LoopDir)}, InnerPoints);
                ['n', 'w'] ->
                    %% loop pipe goes west and bends N
                    calc_inner(Map, {{X+1, Y}, LoopDir}, InnerPoints)
            end;
        _Dirs when LoopDir =:= 'out' ->
            %% debris
            calc_inner(Map, {{X+1, Y}, LoopDir}, InnerPoints);
        _Dirs when LoopDir =:= 'in' ->
            %% debris, but its ours
            calc_inner(Map, {{X+1, Y}, LoopDir}, [{X, Y} | InnerPoints])
    end.

not_loop_dir('out') -> 'in';
not_loop_dir('in') -> 'out'.

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
