-module(day8).

-export([count_steps/1
        ,count_simultaneous_steps/1
        ]).

-define(START, <<"AAA">>).
-define(END, <<"ZZZ">>).
-define(START2, <<_:2/binary, "A">>).
-define(END2, <<_:2/binary, "Z">>).

count_steps(Input) ->
    {Instructions, Tree} = parse_input(Input),
    EndCond = fun(Node) -> Node =:= ?END end,
    Steps = steps(Instructions, Tree, [?START], EndCond),
    lists:foldl(fun lcm/2, 1, Steps).

count_simultaneous_steps(Input) ->
    {Instructions, Tree} = parse_input(Input),
    Starts = [Key || ?START2=Key <- maps:keys(Tree)],
    EndCond = fun(?END2) -> 'true'; (_) -> 'false' end,
    Steps = steps(Instructions, Tree, Starts, EndCond),
    lists:foldl(fun lcm/2, 1, Steps).

steps(Instructions, Tree, Starts, EndCond) ->
    [steps(Instructions, Tree, Start, EndCond, Instructions, 0)
     || Start <- Starts
    ].

steps([], Tree, CurNode, EndCond, Instructions0, Steps) ->
    steps(Instructions0, Tree, CurNode, EndCond, Instructions0, Steps);
steps([Instruction | Instructions], Tree, CurNode, EndCond, Instructions0, Steps) ->
    case EndCond(CurNode) of
        'true' -> Steps;
        'false' ->
            NextNode = next_node(Instruction, Tree, CurNode),
            steps(Instructions, Tree, NextNode, EndCond, Instructions0, Steps + 1)
    end.

next_node(Instruction, Tree, Node) ->
    {Left, Right} = maps:get(Node, Tree),
    case Instruction of
        $L -> Left;
        $R -> Right
    end.

lcm(A, B) -> A * B div gcd(A, B).

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

parse_input(Input) ->
    [InstructionsInput, TreeInput] = binary:split(Input, <<"\n\n">>, ['global']),
    Instructions = parse_instructions(InstructionsInput),
    Tree = parse_tree(TreeInput),
    {Instructions, Tree}.

parse_instructions(Input) -> [Instruction || <<Instruction>> <= Input].

parse_tree(Input) ->
    Nodes = binary:split(Input, <<"\n">>, ['global']),
    lists:foldl(fun add_node/2, #{}, Nodes).

add_node(<<Key:3/binary, " = (", Left:3/binary, ", ", Right:3/binary, ")">>, Acc) ->
    Acc#{Key => {Left, Right}}.
