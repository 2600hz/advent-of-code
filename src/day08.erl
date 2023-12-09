-module(day08).

-export([run/0]).

%% https://adventofcode.com/2023/day/8/
%% steps to finish network: 23147
%% all steps lead to Z: 22289513667691

run() ->
    Network = parse_network(input("day08.txt")),
    part1(Network),
    part2(Network).

part1(Network) ->
    Steps = count_steps(Network, <<"AAA">>),
    io:format("steps to finish network: ~p~n", [Steps]).

count_steps({StepOrder, Nodes}, Start) ->
    count_steps({StepOrder, StepOrder}, Nodes, {Start, 0}).

count_steps(_, _, Steps) when is_integer(Steps) -> Steps;
count_steps({StepOrder, []}, Nodes, {Start, Steps}) ->
    count_steps({StepOrder, StepOrder}, Nodes, {Start, Steps});
count_steps({StepOrder, [NextStep | NextSteps]}, Nodes, {Current, Steps}) ->
    count_steps({StepOrder, NextSteps}
               ,Nodes
               ,take_step(Current, NextStep, Nodes, Steps)
               ).

take_step(Current, NextNode, Nodes, Steps) ->
    case maps:get(Current, Nodes) of
        {<<_:2/binary, $Z>>, _} when NextNode =:= $L -> Steps+1;
        {_, <<_:2/binary, $Z>>} when NextNode =:= $R -> Steps+1;
        {L, _} when NextNode =:= $L -> {L, Steps+1};
        {_, R} when NextNode =:= $R -> {R, Steps+1}
    end.

part2({_Steps, Nodes}=Network) ->
    StartingNodes = maps:fold(fun(<<_:2/binary, $A>>=S, _, Ss) -> [S|Ss];
                                 (_, _, Acc) -> Acc
                              end
                             ,[]
                             ,Nodes
                             ),

    Steps = [count_steps(Network, StartingNode) || StartingNode <- StartingNodes],
    io:format("all steps lead to Z: ~p~n", [lcm(Steps)]).

%% Euler's Algorithm
gcd(A, 0) -> A;
gcd(A, B) when A < 0 orelse B < 0 -> gcd(abs(A), abs(B));
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, B) -> gcd(B, A - B * (A div B)).

%% gcd(A, B, C) = gcd( gcd(a,b), c)
gcd([A, B | L]) ->
    lists:foldl(fun(X, Acc) -> gcd(X, Acc) end, gcd(A, B), L).

lcm(A, B) ->
    A * B div gcd(A, B).
%% lcm(A, B, C) = lcm( lcm(a,b), c)
lcm([A, B | L]) ->
    lists:foldl(fun(X, Acc) -> lcm(X, Acc) end, lcm(A, B), L).

input(File) ->
    {'ok', Bin} = file:read_file(filename:join(["src", File])),
    Bin.

parse_network(Input) ->
    [StepsBin, <<>> | NodeBins] = binary:split(Input, <<$\n>>, ['global', 'trim']),
    {[Step || <<Step>> <= StepsBin]
    ,parse_nodes(NodeBins)
    }.

parse_nodes(NodeBins) ->
    lists:foldl(fun parse_node/2, #{}, NodeBins).

parse_node(NodeBin, Network) ->
    [Node, EdgesBin] = binary:split(NodeBin, <<" = ">>),
    [<<"(", LeftEdge/binary>>, RightEdge] = binary:split(EdgesBin, <<", ">>),
    RightSize = byte_size(RightEdge)-1,
    <<Right:RightSize/binary, _:1/binary>> = RightEdge,
    Network#{Node => {LeftEdge, Right}}.
