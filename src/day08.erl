-module(day08).

-export([run/0]).

%% https://adventofcode.com/2023/day/8/
%% steps to finish network: 23147


run() ->
    Network = parse_network(input("day08.txt")),
    part1(Network),
    part2(Network).

part1(Network) ->
    Steps = count_steps(Network, <<"AAA">>, <<"ZZZ">>),
    io:format("steps to finish network: ~p~n", [Steps]).

count_steps({StepOrder, Nodes}, Start, Finish) ->
    count_steps({StepOrder, StepOrder}, Nodes, Start, Finish, 0).

count_steps(_StepOrders, _Nodes, Finish, Finish, Steps) -> Steps;
count_steps({StepOrder, []}, Nodes, Start, Finish, Steps) ->
    count_steps({StepOrder, StepOrder}, Nodes, Start, Finish, Steps);
count_steps({StepOrder, [NextStep | NextSteps]}, Nodes, Current, Finish, Steps) ->
    count_steps({StepOrder, NextSteps}, Nodes, take_step(Current, NextStep, Nodes), Finish, Steps+1).

take_step(Current, NextNode, Nodes) ->
    {L, R} = maps:get(Current, Nodes),
    case NextNode of
        $L -> L;
        $R -> R
    end.

part2(Network) ->
    Network.

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
