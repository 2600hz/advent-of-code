-module(day22).

-export([disintegrated_bricks/1
        ,falling_bricks/1
        ]).

-record(brick, {id :: pos_integer()
               ,start_c :: {X :: pos_integer(), Y :: pos_integer(), Z :: pos_integer()}
               ,end_c :: {X :: pos_integer(), Y :: pos_integer(), Z :: pos_integer()}
               ,supports = [] :: [pos_integer()]
               }).

disintegrated_bricks(Input) ->
    {W, L, Bricks} = parse_input(Input),
    Bricks1 = lists:sort(fun compare_z_desc/2, Bricks),
    Bricks2 = stack_bricks({W, L, Bricks1}),
    BrickSupports = [Brick#brick.supports || Brick <- Bricks2],
    {SingleSupported, MultiSupported} =
        lists:partition(fun(Supports) -> length(Supports) =:= 1 end, BrickSupports),
    %% Bricks exclusively supporting a brick
    SingleSupports = lists:flatten(SingleSupported),
    %% Bricks sharing support of a brick
    MultiSupports = lists:uniq(lists:flatten(MultiSupported)),
    %% Bricks sharing support of a brick and not exclusively supporting another
    %% brick
    MultiSupports1 = [Support || Support <- MultiSupports,
                                 not lists:member(Support, SingleSupports)
                     ],
    %% Bricks that are not supporting any other bricks
    NonSupporting = [Id || #brick{id = Id} <- Bricks2,
                           not lists:member(Id, SingleSupports ++ MultiSupports)
                    ],
    length(MultiSupports1) + length(NonSupporting).

falling_bricks(Input) ->
    {W, L, Bricks} = parse_input(Input),
    Bricks1 = lists:sort(fun compare_z_desc/2, Bricks),
    Bricks2 = lists:reverse(stack_bricks({W, L, Bricks1})),
    falling_bricks(Bricks2, #{}).

falling_bricks([], Acc) -> lists:sum(maps:values(Acc));
falling_bricks([#brick{id = Id} | Bricks], Acc) ->
    Acc1 = Acc#{Id => chain_reaction([Id], Bricks)},
    falling_bricks(Bricks, Acc1).

chain_reaction(Id, Bricks) -> chain_reaction(Id, Bricks, 0).

chain_reaction([], _, Count) -> Count;
chain_reaction([Id | Ids], Bricks, Count) ->
    {Supported, Unsupported} =
        lists:partition(fun(#brick{supports = Supports}) ->
                                lists:member(Id, Supports)
                        end, Bricks),
    %% Remaining bricks without support of current brick
    Updated = [Brick#brick{supports = lists:delete(Id, Supports)}
               || #brick{supports = Supports}=Brick <- Supported
              ],
    %% Remaining bricks that are now unsupported
    NewUnsupportedIds = [Id1 || #brick{id = Id1, supports = Supports} <- Updated,
                                length(Supports) =:= 0
                        ],
    %% Recurse with unsupported bricks
    chain_reaction(NewUnsupportedIds ++ Ids
                  ,Updated ++ Unsupported
                  ,Count + length(NewUnsupportedIds)
                  ).

compare_z_desc(#brick{start_c = StartC1, end_c = EndC1},
               #brick{start_c = StartC2, end_c = EndC2}) ->
    compare_z_desc({StartC1, EndC1}, {StartC2, EndC2});
compare_z_desc({{_, _, SZ1}, {_, _, EZ1}}, {{_, _, SZ2}, {_, _, EZ2}}) ->
    min(SZ1, EZ1) < min(SZ2, EZ2).

stack_bricks({W, L, Bricks}) ->
    Stacks = initialize_stacks(W, L),
    {_, Bricks1} = lists:foldl(fun stack_brick/2, {Stacks, []}, Bricks),
    Bricks1.

stack_brick(Brick, {Stacks, Settled}) ->
    Ranges = ranges(Brick),
    {Brick1, Stacks1} = settle(Brick, Ranges, Stacks),
    {Stacks1, [Brick1 | Settled]}.

ranges(#brick{start_c = StartC, end_c = EndC}) -> ranges({StartC, EndC});
ranges({{SX, SY, SZ}, {EX, EY, EZ}}=Brick) ->
    case orientation(Brick) of
        'vertical' ->
            Zs = lists:seq(min(SZ, EZ), max(SZ, EZ)),
            [{SX, SY, Z} || Z <- Zs];
        'horizontal' ->
            Xs = lists:seq(SX, EX),
            Ys = lists:seq(SY, EY),
            [{X, Y, SZ} || X <- Xs, Y <- Ys]
    end.

settle(#brick{id = Id, start_c = {SX, SY, SZ}, end_c = {EX, EY, EZ}}=Brick, Ranges, Stacks) ->
    {MaxAvailZ, Supports} = max_avail_z(Ranges, Stacks),
    %% Find the bottom of the brick to stack on the `MaxAvailZ'
    MinZ = min(SZ, EZ),
    Diff = MinZ - MaxAvailZ,
    %% Reduce the Z-index of the brick to make it "fall"
    Brick1 = Brick#brick{start_c = {SX, SY, SZ - Diff}
                        ,end_c = {EX, EY, EZ - Diff}
                        ,supports = Supports
                        },
    %% Update the stacks so future bricks can be stacked on top of this brick
    Ranges1 = [{Id, X, Y, Z - Diff} || {X, Y, Z} <- Ranges],
    Stacks1 = lists:foldl(fun update_stack/2, Stacks, Ranges1),
    {Brick1, Stacks1}.

%%------------------------------------------------------------------------------
%% @doc Find the highest available z-coordinate for a brick and the other bricks
%% that would support that brick.
%% @end
%%------------------------------------------------------------------------------
max_avail_z(Ranges, Stacks) -> max_avail_z(Ranges, Stacks, {1, []}).

max_avail_z([], _, {MaxAvailZ, Supports}) ->
    Supports1 = lists:uniq([Support || {Z, Support} <- Supports,
                                       Z =:= MaxAvailZ,
                                       Support =/= 'undefined'
                           ]),
    {MaxAvailZ, Supports1};
max_avail_z([{X, Y, _} | Ranges], Stacks, {MaxAvailZ, Supports}) ->
    Row = array:get(Y, Stacks),
    {StackSize, BrickId} = array:get(X, Row),
    max_avail_z(Ranges, Stacks, {max(MaxAvailZ, StackSize + 1)
                                ,[{StackSize + 1, BrickId} | Supports]
                                }).

update_stack({Id, X, Y, Z}, Stacks) ->
    Row = array:get(Y, Stacks),
    Row1 = array:set(X, {Z, Id}, Row),
    array:set(Y, Row1, Stacks).

orientation({{_, _, SZ}, {_, _, SZ}}) -> 'horizontal';
orientation({_, _}) -> 'vertical'.

initialize_stacks(W, L) ->
    array:from_list(
      lists:duplicate(L, array:from_list(lists:duplicate(W, {0, 'undefined'})))
     ).

parse_input(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    Bricks = parse_bricks(Lines),
    {lists:max([brick_max(Brick, 'x') || Brick <- Bricks]) + 1
    ,lists:max([brick_max(Brick, 'y') || Brick <- Bricks]) + 1
    ,Bricks
    }.

parse_bricks(Lines) -> parse_bricks(Lines, 1, []).

parse_bricks([], _, Acc) -> lists:reverse(Acc);
parse_bricks([Line | Lines], Id, Acc) ->
    Brick = parse_brick(Line, Id),
    parse_bricks(Lines, Id + 1, [Brick | Acc]).

parse_brick(BrickBin, Id) ->
    [Start, End] = binary:split(BrickBin, <<$~>>),
    {{SX, SY, SZ}, {EX, EY, EZ}}=Coords = {parse_coords(Start), parse_coords(End)},
    {Start1, End1} = case SZ =< EZ of
                         'true' -> Coords;
                         'false' -> {{EX, EY, EZ}, {SX, SY, SZ}}
                     end,
    #brick{id = Id, start_c = Start1, end_c = End1}.

parse_coords(Coords) ->
    [X, Y, Z] = binary:split(Coords, <<",">>, ['global']),
    {binary_to_integer(X)
    ,binary_to_integer(Y)
    ,binary_to_integer(Z)
    }.

brick_max(#brick{start_c = {SX, _, _}, end_c = {EX, _, _}}, 'x') -> max(SX, EX);
brick_max(#brick{start_c = {_, SY, _}, end_c = {_, EY, _}}, 'y') -> max(SY, EY).
