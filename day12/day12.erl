%%%=============================================================================
%%% @doc Advent of Code 2023 Day 12
%%%
%%% Dynamic programming solution.  The cache is a map of `{I, C}' to the number
%%% of arrangements for that state.  The state is the pair of the number of
%%% processed elements and the number of consumed elements.
%%%
%%% @author Daniel Finke <danielfinke2011@gmail.com>
%%% @end
%%%=============================================================================
-module(day12).

-export([sum_arrangements/2]).

sum_arrangements(Input, Part) ->
    Records = parse_input(Input),
    Records1 = case Part of
                   1 -> Records;
                   2 -> unfold(Records, 5)
               end,
    Arrangements = [process(Record) || Record <- Records1],
    lists:sum(Arrangements).

unfold(Records, Factor) ->
    [unfold(Springs, Sizes, Factor) || {Springs, Sizes} <- Records].

unfold(Springs, Sizes, Factor) ->
    Springs1 = lists:flatten(lists:join("?", lists:duplicate(Factor, Springs))),
    {Springs1, lists:flatten(lists:duplicate(Factor, Sizes))}.

process(Record) ->
    {Arrangements, _Cache} = process(Record, {'false', 1, 0, maps:new()}),
    Arrangements.

%% Use cache (`I': number of processed elements, `C': cache)
process({_Springs, _Sizes}, {_IsConsuming, I, C, Cache})
  when is_map_key({I, C}, Cache) ->
    {maps:get({I, C}, Cache), Cache};
%% Can stop consuming
process({[$? | Rest], [0 | Sizes]}, {'true', I, C, Cache}) ->
    process({Rest, Sizes}, {'false', I + 1, C + 1, Cache});
process({[$. | Rest], [0 | Sizes]}, {'true', I, C, Cache}) ->
    process({Rest, Sizes}, {'false', I + 1, C + 1, Cache});
%% Fail, couldn't consume
process({[$# | _Rest], [0 | _Sizes]}, {'true', _I, _C, Cache}) -> {0, Cache};
process({[$. | _Rest], _Sizes}, {'true', _I, _C, Cache}) -> {0, Cache};
%% Cleanup
process({Springs, [0 | Sizes]}, {IsConsuming, I, C, Cache}) ->
    process({Springs, Sizes}, {IsConsuming, I, C, Cache});
%% Base cases (must be after cleanup)
process({[], []}, {_IsConsuming, _I, _C, Cache}) -> {1, Cache};
process({[], _Sizes}, {_IsConsuming, _I, _C, Cache}) -> {0, Cache};
process({Springs, []}, {_IsConsuming, _I, _C, Cache}) ->
    case lists:member($#, Springs) of
        'true' -> {0, Cache};
        'false' -> {1, Cache}
    end;
%% Continue or begin consuming
process({[$? | Rest], [Size | Sizes]}, {'true', I, C, Cache}) ->
    process({Rest, [Size - 1 | Sizes]}, {'true', I + 1, C + 1, Cache});
process({[$# | Rest], [Size | Sizes]}, {_IsConsuming, I, C, Cache}) ->
    process({Rest, [Size - 1 | Sizes]}, {'true', I + 1, C + 1, Cache});
%% Fork
process({[$? | Rest], [Size | Sizes]}, {'false', I, C, Cache}) ->
    I1 = I + 1,
    %% Consume
    {Left, Cache1} = process({Rest, [Size - 1 | Sizes]}
                            ,{'true', I1, C + 1, Cache}
                            ),
    Cache2 = Cache1#{{I1, C + 1} => Left},
    %% Don't consume
    {Right, Cache3} = process({Rest, [Size | Sizes]}
                             ,{'false', I1, C, Cache2}
                             ),
    Cache4 = Cache3#{{I1, C} => Right},
    {Left + Right, Cache4};
%% Nothing to do
process({[$. | Rest], Sizes}, {'false', I, C, Cache}) ->
    process({Rest, Sizes}, {'false', I + 1, C, Cache}).

parse_input(Input) ->
    Lines = binary:split(Input, <<"\n">>, ['global']),
    [parse_line(Line) || Line <- Lines].

parse_line(Line) ->
    [Springs, Sizes] = binary:split(Line, <<" ">>, ['global']),
    {binary_to_list(Springs), parse_sizes(Sizes)}.

parse_sizes(SizesBin) ->
    Sizes = binary:split(SizesBin, <<",">>, ['global']),
    [binary_to_integer(Size) || Size <- Sizes].
