#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% To completely determine whether you have enough adapters, you'll
%% need to figure out how many different ways they can be
%% arranged. Every arrangement needs to connect the charging outlet to
%% your device. The previous rules about when adapters can successfully
%% connect still apply.

%% The first example above (the one that starts with 16, 10, 15)
%% supports the following arrangements:

%% (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
%% (0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)
%% (0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)
%% (0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)
%% (0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)
%% (0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)
%% (0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
%% (0), 1, 4, 7, 10, 12, 15, 16, 19, (22)

%% (The charging outlet and your device's built-in adapter are shown in
%% parentheses.) Given the adapters from the first example, the total
%% number of arrangements that connect the charging outlet to your
%% device is 8.

%% The second example above (the one that starts with 28, 33, 18) has
%% many arrangements. Here are a few:

%% (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
%% 32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49, (52)

%% (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
%% 32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 49, (52)

%% (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
%% 32, 33, 34, 35, 38, 39, 42, 45, 46, 48, 49, (52)

%% (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
%% 32, 33, 34, 35, 38, 39, 42, 45, 46, 49, (52)

%% (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
%% 32, 33, 34, 35, 38, 39, 42, 45, 47, 48, 49, (52)

%% (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
%% 46, 48, 49, (52)

%% (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
%% 46, 49, (52)

%% (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
%% 47, 48, 49, (52)

%% (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
%% 47, 49, (52)

%% (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
%% 48, 49, (52)

%% In total, this set of adapters can connect the charging outlet to
%% your device in 19208 distinct arrangements.

%% You glance back down at your bag

%% and try to remember why you brought so many adapters; there must be
%% more than a trillion valid ways to arrange them! Surely, there must
%% be an efficient way to count the arrangements.

%% What is the total number of distinct ways you can arrange the
%% adapters to connect the charging outlet to your device?

main(_) ->
    [MaxJolt | _]=Joltages = read_input("p10.txt"), % jolts in reverse
    DeviceJolt = MaxJolt + 3,
    %% io:format("dj: ~p js: ~p~n", [DeviceJolt, Joltages]),

    NewGraph = digraph:new(['acyclic']),
    {_, Graph} = lists:foldl(fun add_edges/2
                            ,add_edges(DeviceJolt, {Joltages, NewGraph})
                            ,Joltages
                            ),
    Counts = count_paths(Graph, DeviceJolt),
    io:format("count: ~p~n", [maps:get(DeviceJolt, Counts)]).

count_paths(Graph, Vertex) ->
    count_paths(Graph, Vertex, #{}).

count_paths(Graph, Vertex, VerticeCounts) ->
    count_paths(Graph, Vertex, VerticeCounts, digraph:out_neighbours(Graph, Vertex)).

count_paths(_Graph, Vertex, VerticeCounts, []) ->
    %% io:format("v:~p no neighbors~n", [Vertex]),
    VerticeCounts#{Vertex => 0};
count_paths(Graph, Vertex, VerticeCounts, Neighbors) ->
    %% io:format("v:~p neighbors: ~p~n", [Vertex, Neighbors]),
    {_, NewCounts} = lists:foldl(fun count_neighbors/2, {Graph, VerticeCounts}, Neighbors),
    {VCount, _} = lists:foldl(fun count_path/2, {0, NewCounts}, Neighbors),
    %% io:format("v:~p count: ~p~n", [Vertex, VCount]),
    VerticeCounts#{Vertex => VCount}.

count_path(NVertex, {Count, VerticeCounts}) ->
    {maps:get(NVertex, VerticeCounts) + Count, VerticeCounts}.

count_neighbors(0, {Graph, VerticeCounts}) ->
    {Graph, VerticeCounts#{0 => 1}};
count_neighbors(NVertex, {Graph, VerticeCounts}) ->
    case maps:get(NVertex, VerticeCounts, 'undefined') of
        'undefined' ->
            %% io:format("nv:~p counting~n", [NVertex]),
            {Graph, count_paths(Graph, NVertex, VerticeCounts)};
        _AlreadyCounted ->
            %% io:format("nv:~p already ~p~n", [NVertex, _AlreadyCounted]),
            {Graph, VerticeCounts}
    end.

add_edges(Jolt, {Jolts, Graph}=Acc) ->
    JVertex = digraph:add_vertex(Graph, Jolt, [Jolt]),
    [add_to_graph(Graph, JVertex, Next)
     || Next <- next_jolts(Jolts, Jolt)
    ],
    Acc.

add_to_graph(Graph, JVertex, Next) ->
    NVertex = digraph:add_vertex(Graph, Next, [Next]),
    digraph:add_edge(Graph, JVertex, NVertex).

next_jolts(Joltages, CurrentJoltage) ->
    [Jolt || Jolt <- Joltages,
            Diff <- [CurrentJoltage-Jolt],
            Diff > 0 andalso Diff < 4 % 1, 2, 3 diff
    ].

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    lists:reverse(
      lists:sort(
        [0 | [binary_to_integer(Line, 10) || Line <- binary:split(Lines, <<"\n">>, ['global', 'trim'])]]
       )
     ).


%% (0) 1 2 3 (6) -> [0 1 2 3 6] [0 1 3 6] [0 2 3 6] [0 3 6]

%% 6 -> [3]
%% 3 -> [2 1 0]
%% 2 -> [1 0]
%% 1 -> [0]

%% 1 -> 1
%% 2 -> 1 + 1
%% 3 -> 2 + 1 + 1
%% 6 -> 4
