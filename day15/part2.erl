-module(part2).

-export([main/0]).

%% --- Part Two ---

%% Impressed, the Elves issue you a challenge: determine the 30000000th
%% number spoken. For example, given the same starting numbers as
%% above:

%%     Given 0,3,6, the 30000000th number spoken is 175594.
%%     Given 1,3,2, the 30000000th number spoken is 2578.
%%     Given 2,1,3, the 30000000th number spoken is 3544142.
%%     Given 1,2,3, the 30000000th number spoken is 261214.
%%     Given 2,3,1, the 30000000th number spoken is 6895259.
%%     Given 3,2,1, the 30000000th number spoken is 18.
%%     Given 3,1,2, the 30000000th number spoken is 362.

%% Given your starting numbers, what will be the 30000000th number
%% spoken?

main() ->
    {NthSpoken, Input} = read_input("p15.txt"),
    {Next, Game} = lists:foldl(fun(N, {I, G}) ->
                                       {I+1, update_game(G, I, N)}
                               end
                              ,{1, init_game('ets')}
                              ,Input
                              ),

    %% io:format("game: ~p~nnext: ~p last spoken: ~p~n", [Game, Next, lists:last(Input)]),

    LastSpoken = play_game(Game, NthSpoken, Next, lists:last(Input)),
    io:format("spoken: ~p~n", [LastSpoken]).

-record(speak, {spoken, turns=[]}).

init_game('map') -> {'map', #{}};
init_game('ets') -> {'ets', ets:new(game, [{keypos, #speak.spoken}, set])}.

play_game(_Game, NthSpoken, TurnNo, PrevSpoken) when TurnNo > NthSpoken ->
    PrevSpoken;
play_game(Game, NthSpoken, TurnNo, PrevSpoken) ->
    %% io:format(" turn ~p: ~p~n", [TurnNo, PrevSpoken]),
    case get_spoken(Game, PrevSpoken) of
        [] ->
            play_game(update_game(Game, TurnNo, 0), NthSpoken, TurnNo+1, 0);
        [Recent] when Recent =:= TurnNo-1 ->
            play_game(update_game(Game, TurnNo, 0), NthSpoken, TurnNo+1, 0);
        [RecentOne, RecentTwo] ->
            Updated = update_game(Game, TurnNo, RecentOne - RecentTwo),
            play_game(Updated, NthSpoken, TurnNo+1, RecentOne - RecentTwo)
    end.

get_spoken({'map', Game}, PrevSpoken) ->
    maps:get(PrevSpoken, Game, []);
get_spoken({'ets', TID}, PrevSpoken) ->
    try ets:lookup_element(TID, PrevSpoken, #speak.turns)
    catch _:_ -> []
    end.

update_game(Game, TurnNo, Spoken) ->
    update_game(Game, TurnNo, Spoken, get_spoken(Game, Spoken)).

update_game({'map', Game}, TurnNo, Spoken, []) ->
    {'map', Game#{Spoken => [TurnNo]}};
update_game({'ets', TID}, TurnNo, Spoken, []) ->
    _In = ets:insert(TID, #speak{spoken=Spoken, turns=[TurnNo]}),
    {'ets', TID};

update_game({'map', Game}, TurnNo, Spoken, [Recent|_]) ->
    {'map', Game#{Spoken => [TurnNo, Recent]}};
update_game({'ets', TID}, TurnNo, Spoken, [Recent|_]) ->
    _In = ets:insert(TID, #speak{spoken=Spoken, turns=[TurnNo, Recent]}),
    {'ets', TID}.

read_input("p15-test.txt") ->
    {10, [0,3,6]};
read_input("p15.txt") ->
    {30000000, [1,12,0,20,8,16]}.
