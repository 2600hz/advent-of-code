#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% The shuttle company is running a contest: one gold coin for anyone
%% that can find the earliest timestamp such that the first bus ID
%% departs at that time and each subsequent listed bus ID departs at
%% that subsequent minute. (The first line in your input is no longer
%% relevant.)

%% For example, suppose you have the same list of bus IDs as above:

%% 7,13,x,x,59,x,31,19

%% An x in the schedule means there are no constraints on what bus IDs
%% must depart at that time.

%% This means you are looking for the earliest timestamp (called t)
%% such that:

%%     Bus ID 7 departs at timestamp t.
%%     Bus ID 13 departs one minute after timestamp t.
%%     There are no requirements or restrictions on departures at two
%%     or three minutes after timestamp t.
%%     Bus ID 59 departs four minutes after timestamp t.
%%     There are no requirements or restrictions on departures at five
%%     minutes after timestamp t.
%%     Bus ID 31 departs six minutes after timestamp t.
%%     Bus ID 19 departs seven minutes after timestamp t.

%% The only bus departures that matter are the listed bus IDs at their
%% specific offsets from t. Those bus IDs can depart at other times,
%% and other bus IDs can depart at those times. For example, in the
%% list above, because bus ID 19 must depart seven minutes after the
%% timestamp at which bus ID 7 departs, bus ID 7 will always also be
%% departing with bus ID 19 at seven minutes after timestamp t.

%% In this example, the earliest timestamp at which this occurs is
%% 1068781:

%% time     bus 7   bus 13  bus 59  bus 31  bus 19
%% 1068773    .       .       .       .       .
%% 1068774    D       .       .       .       .
%% 1068775    .       .       .       .       .
%% 1068776    .       .       .       .       .
%% 1068777    .       .       .       .       .
%% 1068778    .       .       .       .       .
%% 1068779    .       .       .       .       .
%% 1068780    .       .       .       .       .
%% 1068781    D       .       .       .       .
%% 1068782    .       D       .       .       .
%% 1068783    .       .       .       .       .
%% 1068784    .       .       .       .       .
%% 1068785    .       .       D       .       .
%% 1068786    .       .       .       .       .
%% 1068787    .       .       .       D       .
%% 1068788    D       .       .       .       D
%% 1068789    .       .       .       .       .
%% 1068790    .       .       .       .       .
%% 1068791    .       .       .       .       .
%% 1068792    .       .       .       .       .
%% 1068793    .       .       .       .       .
%% 1068794    .       .       .       .       .
%% 1068795    D       D       .       .       .
%% 1068796    .       .       .       .       .
%% 1068797    .       .       .       .       .

%% In the above example, bus ID 7 departs at timestamp 1068788 (seven
%% minutes after t). This is fine; the only requirement on that minute
%% is that bus ID 19 departs then, and it does.

%% Here are some other examples:

%%     The earliest timestamp that matches the list 17,x,13,19 is 3417.
%%     67,7,59,61 first occurs at timestamp 754018.
%%     67,x,7,59,61 first occurs at timestamp 779210.
%%     67,7,x,59,61 first occurs at timestamp 1261476.
%%     1789,37,47,1889 first occurs at timestamp 1202161486.

%% However, with so many bus IDs in your list, surely the actual
%% earliest timestamp will be larger than 100000000000000!

%% What is the earliest timestamp such that all of the listed bus IDs
%% depart at offsets matching their positions in the list?

main(_) ->
    %% {_Depart, BusIds} = {ok, [17, <<"x">>, 13, 19]}, %read_input("p13.txt"),
    %% {_Depart, BusIds} = {ok, [67,7,59,61]},
    {_Depart, BusIds} = {ok, [3, <<"x">>, <<"x">>, 4, 5]}, %read_input("p13.txt"),
    UpperBound = lists:foldl(fun(<<"x">>, Acc) -> Acc; (N, Acc) -> N * Acc end, 1, BusIds), % 4199
    {_, BusOffsets} = lists:foldl(fun bus_offset/2, {0, []}, BusIds), % [{17, 0}, {13, 2}, {19, 3}]

    %% BusOffsets = [{3, 2}, {4, 3}, {5, 1}],
    %% UpperBound = 3*4*5,
    T = chinese_remainder(UpperBound, lists:reverse(BusOffsets)),

    io:format("ub: ~p t: ~p(~p)~n", [UpperBound, T, is_ans(T, BusOffsets)]).

is_ans(T, BusOffsets) ->
    lists:all(fun({Bus, Offset}) ->
                      0 =:= (T+Offset) rem Bus
              end
             ,BusOffsets
             ).

bus_offset(<<"x">>, {Offset, Acc}) -> {Offset+1, Acc};
bus_offset(BusId, {Offset, Acc}) -> {Offset+1, [{BusId, Offset} | Acc]}.

read_input(File) ->
    {'ok', Lines} = file:read_file(File),
    [Depart, Buses] = binary:split(Lines, <<"\n">>, ['global', 'trim']),
    {binary_to_integer(Depart, 10)
    ,[parse_bus_id(BusId) || BusId <- binary:split(Buses, <<",">>, ['global', 'trim'])]
    }.


parse_bus_id(<<"x">>) -> <<"x">>;
parse_bus_id(BusId) -> binary_to_integer(BusId).


%% Euler's Algorithm
gcd(A, 0) -> A;
gcd(A, B) when A < 0 orelse B < 0 -> gcd(abs(A), abs(B));
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, B) -> gcd(B, A - B * (A div B)).

%% gcd(A, B, C) = gcd( gcd(a,b), c)
gcd([A, B | L]) ->
    lists:foldl(fun(X, Acc) -> gcd(X, Acc) end, gcd(A, B), L).

egcd(A, B) ->
    egcd(A, B, 1, 0, 0, 1).

egcd(OldR, 0, OldS, S, OldT, T) ->
    {{bezout, OldS, OldT}
    ,{gcd, OldR}
    ,{quotients, T, S}
    };
egcd(OldR, R, OldS, S, OldT, T) ->
    Q = OldR div R,
    egcd(R, OldR - (Q * R)
        ,S, OldS - (Q * S)
        ,T, OldT - (Q * T)
        ).

constructive_proof({N1, A1}, {N2, A2}) ->
    {{bezout, M1, M2}
    ,{gcd, _GCD}
    ,_Q
    } = egcd(N1, N2),
    %% io:format("n1: ~p a1: ~p m1: ~p~nn2: ~p a2: ~p m2: ~p~n gcd: ~p~n  bi: ~p * ~p + ~p * ~p = ~p~n"
    %%          ,[N1, A1, M1
    %%           ,N2, A2, M2
    %%           ,_GCD
    %%           ,M1,N1, M2, N2, (M1*N1) + (M2*N2)
    %%           ]
    %%          ),
    CP = (M1 * N1 * A2) + (M2 * N2 * A1),
    io:format("    constr proof: (~p * ~p * ~p) + (~p * ~p * ~p): ~p ~p~n"
             ,[M1, N1, A2, M2, N2, A1, _Q, CP]
             ),
    CP.

chinese_remainder(_UpperBound, [{_N, A}]) -> A;
chinese_remainder(UpperBound, [{N1, _}=X, {N2, _}=Y | Rest]) ->
    A1_2 = constructive_proof(X, Y),
    N1_2 = N1 * N2,

    lists:foldl(fun(MultipleOfN, Min) ->
                        M = chinese_remainder(UpperBound, [{N1_2, MultipleOfN+A1_2} | Rest]),
                        case abs(M) of
                            AbsM when AbsM < Min ->
                                io:format(" {~p, ~p+~p} new min ~p~n", [N1_2, MultipleOfN, A1_2, AbsM]),
                                AbsM;
                            _AbsM ->
                                io:format(" {~p, ~p+~p} not ~p~n", [N1_2, MultipleOfN, A1_2, _AbsM]),
                                Min
                        end
                end
               ,UpperBound
               ,[-N1_2, 0, N1_2]
               ).
