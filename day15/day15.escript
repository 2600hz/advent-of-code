#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 15: Chiton ---

%% You've almost reached the exit of the cave, but the walls are
%% getting closer together. Your submarine can barely still fit,
%% though; the main problem is that the walls of the cave are covered
%% in chitons, and it would be best not to bump any of them.

%% The cavern is large, but has a very low ceiling, restricting your
%% motion to two dimensions. The shape of the cavern resembles a
%% square; a quick scan of chiton density produces a map of risk level
%% throughout the cave (your puzzle input). For example:

%% 1163751742
%% 1381373672
%% 2136511328
%% 3694931569
%% 7463417111
%% 1319128137
%% 1359912421
%% 3125421639
%% 1293138521
%% 2311944581

%% You start in the top left position, your destination is the bottom
%% right position, and you cannot move diagonally. The number at each
%% position is its risk level; to determine the total risk of an
%% entire path, add up the risk levels of each position you enter
%% (that is, don't count the risk level of your starting position
%% unless you enter it; leaving it adds no risk to your total).

%% Your goal is to find a path with the lowest total risk. In this
%% example, a path with the lowest total risk is highlighted here:

%% 1163751742
%% 1381373672
%% 2136511328
%% 3694931569
%% 7463417111
%% 1319128137
%% 1359912421
%% 3125421639
%% 1293138521
%% 2311944581

%% The total risk of this path is 40 (the starting position is never
%% entered, so its risk is not counted).

%% What is the lowest total risk of any path from the top left to the
%% bottom right?

main(_) ->
    Input = read_input("p15.txt"),
    p15_1(Input),
    p15_2(Input).

p15_1(#{max_x:=MaxX
       ,max_y:=MaxY
       }=Risks
     ) ->
    Path = a_star(Risks, {0, 0}, {MaxX, MaxY}),
    io:format("path cost: ~p: ~w~n", [lists:sum(Path), Path]).

-record(node, {xy, cost, parent, f}).

a_star(Risks, Start, End) ->
    OpenNodes = [#node{xy=Start, cost=maps:get(Start, Risks), parent='undefined', f=0}],
    ClosedNodes = [],
    a_star(Risks, Start, End, OpenNodes, ClosedNodes).

a_star(_Risks, _Start, _End, [], ClosedNodes) ->
    io:format("no path through~n", []),
    lists:foldl(fun(#node{cost=Cost}, Acc) -> [Cost | Acc] end, [], ClosedNodes);
a_star(_Risks, Start, End, [#node{xy=End}=Node | _OpenNodes], _ClosedNodes) ->
    end_goal(Start, Node);
a_star(Risks, Start, End, OpenNodes, ClosedNodes) ->
    [#node{xy={X, Y}}=CurrentNode | ONs] = lists:keysort(#node.f, OpenNodes),
    CNs = [CurrentNode | ClosedNodes],

    Neighbors = [{X-1, Y}, {X+1, Y}, {X, Y-1}, {X, Y+1}],

    {Risks, Start, End, ONs1, _} =
        lists:foldl(fun check_neighbor/2
                   ,{Risks, Start, End, ONs, CNs}
                   ,Neighbors
                   ),
    a_star(Risks, Start, End, lists:keysort(#node.f, ONs1), CNs).

check_neighbor({NX, NY}, {Risks, Start, End, OpenNodes, ClosedNodes}=Acc) ->
    case maps:get({NX, NY}, Risks, 'undefined') of
        'undefined' -> Acc;
        Cost ->
            #node{f=PF}=Parent = hd(ClosedNodes),
            Neighbor = #node{xy={NX, NY}
                            ,cost=Cost
                            ,parent=Parent
                            ,f=Cost+PF%% heuristic(Start, End, {NX, NY}, Cost)
                            },

            case lists:keyfind({NX, NY}, #node.xy, ClosedNodes) of
                #node{} -> Acc;
                'false' ->
                    {Risks, Start, End, maybe_add_neighbor(OpenNodes, Neighbor), ClosedNodes}
            end
    end.

maybe_add_neighbor(OpenNodes, #node{xy=NXY, f=NF}=Neighbor) ->
    case lists:any(fun(#node{xy=OpenXY, f=OpenF}) ->
                           OpenXY =:= NXY andalso NF >= OpenF
                   end
                  ,OpenNodes
                  )
    of
        'true' -> OpenNodes;
        'false' -> [Neighbor | OpenNodes]
    end.

%% heurisitc: manhattan distance
%% heuristic({StartX, StartY}, {EndX, EndY}, {NX, NY}, _Cost) ->
%%     G = abs(NX - StartX) + abs(NY - StartY), % cost so far
%%     H = abs(NX - EndX) + abs(NY - EndY), % estimated cost
%%     G + H. % total estimated cost of this path

%% heuristic: euclidean distance
%% heuristic({StartX, StartY}, {EndX, EndY}, {NX, NY}, Cost) ->
%%     math:sqrt(math:pow(StartX-NX, 2)+math:pow(StartY-NY, 2))
%%         + math:sqrt(math:pow(NX-EndX, 2)+math:pow(NY-EndY, 2))
%%         + Cost.

end_goal(Start, CurrentNode) ->
    end_goal(Start, CurrentNode, []).

end_goal(Start, #node{xy=Start}, Path) ->
    Path;
end_goal(Start, #node{cost=Cost, parent=Parent}, Path) ->
    end_goal(Start, Parent, [Cost | Path]).

p15_2(Input) ->
    Input.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    read_risks(binary:split(Lines, <<"\n">>, ['global']), #{}, 0).

read_risks([], Risks, MaxY) ->
    Risks#{max_y => MaxY-1};
read_risks([<<>> | Lines], Risks, Y) ->
    read_risks(Lines, Risks, Y);
read_risks([Line | Lines], Risks, Y) ->
    read_risks(Lines, read_risk(Line, Risks, Y, 0), Y+1).

read_risk(<<>>, Risks, _Y, MaxX) ->
    Risks#{max_x => MaxX-1};
read_risk(<<N, Rest/binary>>, Risks, Y, X) ->
    read_risk(Rest, Risks#{{X, Y} => N-$0}, Y, X+1).
