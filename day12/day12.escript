#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 12: Passage Pathing ---

%% With your submarine's subterranean subsystems subsisting
%% suboptimally, the only way you're getting out of this cave anytime
%% soon is by finding a path yourself. Not just a path - the only way
%% to know if you've found the best path is to find all of them.

%% Fortunately, the sensors are still mostly working, and so you build
%% a rough map of the remaining caves (your puzzle input). For
%% example:

%% start-A
%% start-b
%% A-c
%% A-b
%% b-d
%% A-end
%% b-end

%% This is a list of how all of the caves are connected. You start in
%% the cave named start, and your destination is the cave named
%% end. An entry like b-d means that cave b is connected to cave d -
%% that is, you can move between them.

%% So, the above cave system looks roughly like this:

%%     start
%%     /   \
%% c--A-----b--d
%%     \   /
%%      end

%% Your goal is to find the number of distinct paths that start at
%% start, end at end, and don't visit small caves more than
%% once. There are two types of caves: big caves (written in
%% uppercase, like A) and small caves (written in lowercase, like
%% b). It would be a waste of time to visit any small cave more than
%% once, but big caves are large enough that it might be worth
%% visiting them multiple times. So, all paths you find should visit
%% small caves at most once, and can visit big caves any number of
%% times.

%% Given these rules, there are 10 paths through this example cave
%% system:

%% start,A,b,A,c,A,end
%% start,A,b,A,end
%% start,A,b,end
%% start,A,c,A,b,A,end
%% start,A,c,A,b,end
%% start,A,c,A,end
%% start,A,end
%% start,b,A,c,A,end
%% start,b,A,end
%% start,b,end

%% (Each line in the above list corresponds to a single path; the
%% caves visited by that path are listed in the order they are visited
%% and separated by commas.)

%% Note that in this cave system, cave d is never visited by any path:
%% to do so, cave b would need to be visited twice (once on the way to
%% cave d and a second time when returning from cave d), and since
%% cave b is small, this is not allowed.

%% Here is a slightly larger example:

%% dc-end
%% HN-start
%% start-kj
%% dc-start
%% dc-HN
%% LN-dc
%% HN-end
%% kj-sa
%% kj-HN
%% kj-dc

%% The 19 paths through it are as follows:

%% start,HN,dc,HN,end
%% start,HN,dc,HN,kj,HN,end
%% start,HN,dc,end
%% start,HN,dc,kj,HN,end
%% start,HN,end
%% start,HN,kj,HN,dc,HN,end
%% start,HN,kj,HN,dc,end
%% start,HN,kj,HN,end
%% start,HN,kj,dc,HN,end
%% start,HN,kj,dc,end
%% start,dc,HN,end
%% start,dc,HN,kj,HN,end
%% start,dc,end
%% start,dc,kj,HN,end
%% start,kj,HN,dc,HN,end
%% start,kj,HN,dc,end
%% start,kj,HN,end
%% start,kj,dc,HN,end
%% start,kj,dc,end

%% Finally, this even larger example has 226 paths through it:

%% fs-end
%% he-DX
%% fs-he
%% start-DX
%% pj-DX
%% end-zg
%% zg-sl
%% zg-pj
%% pj-he
%% RW-he
%% fs-DX
%% pj-RW
%% zg-RW
%% start-pj
%% he-WI
%% zg-he
%% pj-fs
%% start-RW

%% How many paths through this cave system are there that visit small
%% caves at most once?

%% --- Part Two ---

%% After reviewing the available paths, you realize you might have
%% time to visit a single small cave twice. Specifically, big caves
%% can be visited any number of times, a single small cave can be
%% visited at most twice, and the remaining small caves can be visited
%% at most once. However, the caves named start and end can only be
%% visited exactly once each: once you leave the start cave, you may
%% not return to it, and once you reach the end cave, the path must
%% end immediately.

%% Now, the 36 possible paths through the first example above are:

%% start,A,b,A,b,A,c,A,end
%% start,A,b,A,b,A,end
%% start,A,b,A,b,end
%% start,A,b,A,c,A,b,A,end
%% start,A,b,A,c,A,b,end
%% start,A,b,A,c,A,c,A,end
%% start,A,b,A,c,A,end
%% start,A,b,A,end
%% start,A,b,d,b,A,c,A,end
%% start,A,b,d,b,A,end
%% start,A,b,d,b,end
%% start,A,b,end
%% start,A,c,A,b,A,b,A,end
%% start,A,c,A,b,A,b,end
%% start,A,c,A,b,A,c,A,end
%% start,A,c,A,b,A,end
%% start,A,c,A,b,d,b,A,end
%% start,A,c,A,b,d,b,end
%% start,A,c,A,b,end
%% start,A,c,A,c,A,b,A,end
%% start,A,c,A,c,A,b,end
%% start,A,c,A,c,A,end
%% start,A,c,A,end
%% start,A,end
%% start,b,A,b,A,c,A,end
%% start,b,A,b,A,end
%% start,b,A,b,end
%% start,b,A,c,A,b,A,end
%% start,b,A,c,A,b,end
%% start,b,A,c,A,c,A,end
%% start,b,A,c,A,end
%% start,b,A,end
%% start,b,d,b,A,c,A,end
%% start,b,d,b,A,end
%% start,b,d,b,end
%% start,b,end

%% The slightly larger example above now has 103 paths through it, and
%% the even larger example now has 3509 paths through it.

%% Given these new rules, how many paths through this cave system are
%% there?

main(_) ->
    Input = read_input("p12-tiny.txt"),
    p12_1(Input),
    p12_2(Input).

p12_1(Graph) ->
    Paths = calc_paths(Graph, <<"start">>, []),
    io:format("found ~p paths~n", [length(Paths)]).
%%     lists:foldl(fun pp_paths/2, 1, Paths).

%% pp_paths(Path, N) ->
%%     io:format("~p: ~s~n", [N, pp_path(Path)]),
%%     N+1.

calc_paths(Graph, Vertex, Paths) ->
    calc_paths(Graph, Vertex, Paths, [], maps:get(Vertex, Graph)).

calc_paths(_Graph, _Vertex, Paths, _CurrentPath, []) ->
    Paths;
%% calc_paths(Graph, Vertex, Paths, CurrentPath, [<<"start">> | Verticies]) ->
%%     calc_paths(Graph, Vertex, Paths, CurrentPath, Verticies);
calc_paths(Graph, Vertex, Paths, CurrentPath, [<<"end">> | Verticies]) ->
    Path = lists:reverse([{Vertex, <<"end">>} | CurrentPath]),
    calc_paths(Graph, Vertex, [Path | Paths], CurrentPath, Verticies);
calc_paths(Graph, Vertex, Paths, CurrentPath, [NextVertex | Verticies]) ->
    NewPath = [{Vertex, NextVertex} | CurrentPath],
    NextVerticies = next_verticies(NextVertex, NewPath, Graph),

    case calc_paths(Graph, NextVertex
                   ,[]
                   ,NewPath
                   ,NextVerticies
                   )
    of
        [] ->
            calc_paths(Graph, Vertex, Paths, NewPath, Verticies);
        NextPaths ->
            calc_paths(Graph, Vertex, NextPaths ++ Paths, NewPath, Verticies)
    end.

next_verticies(Vertex
              ,CurrentPath
              ,Graph
              ) ->
    {Vs, _, _} = lists:foldl(fun maybe_remove_small_cave/2
                            ,{[], Vertex, CurrentPath}
                            ,maps:get(Vertex, Graph)
                            ),
    Vs.

has_small_cave_been_visited(<<L:8, _/binary>> = Vertex, CurrentPath)
  when L >= $a andalso L =< $z ->
    case {lists:keyfind(Vertex, 1, CurrentPath) % visiting
         ,lists:keyfind(Vertex, 2, CurrentPath) % visited
         }
    of
        {{_, _}, {_, _}}=_F ->
            'true';
        _F ->
            'false'
    end;
has_small_cave_been_visited(_BigCave, _CurrentPath) ->
    'false'.

maybe_remove_small_cave(<<"start">>, {Vs, Vertex, CurrentPath}) ->
    {Vs, Vertex, CurrentPath};
maybe_remove_small_cave(NextVertex, {Vs, Vertex, CurrentPath}) ->
    case has_small_cave_been_visited(NextVertex, CurrentPath) of
        'true' -> {Vs, Vertex, CurrentPath};
        'false' -> {[NextVertex | Vs], Vertex, CurrentPath}
    end.

p12_2(Input) ->
    Input.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    lists:foldl(fun add_edge/2, #{}, binary:split(Lines, <<"\n">>, ['global'])).

add_edge(<<>>, Graph) -> Graph;
add_edge(Line, Graph) ->
    [LHS, RHS] = binary:split(Line, <<"-">>),
    LEdges = maps:get(LHS, Graph, []),
    REdges = maps:get(RHS, Graph, []),
    Graph#{LHS => lists:usort([RHS | LEdges])
          ,RHS => lists:usort([LHS | REdges])
          }.

pp_path([]) -> "";
pp_path(Path) ->
    string:join(["start" | [binary_to_list(V) || {_, V} <- Path]], ",").
