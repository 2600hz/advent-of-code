#!/usr/bin/env escript
%%! +A0 -sname day5
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([]) ->
    {'ok', Input} = file:read_file("puzzle.txt"),
    %% {'ok', Input} = file:read_file("sample.txt"),

    {PageOrderRules, PageUpdates} = parse_input(Input),
    %% io:format("rules: ~p~n", [PageOrderRules]),

    {Valid, Invalid} = lists:partition(fun(PU) -> is_valid_update(PU, PageOrderRules) end
                                      ,PageUpdates
                                      ),
    MidEls = [lists:nth(Len div 2 + 1, V) || V <- Valid, Len <- [length(V)]],
    io:format("valid sum: ~w~n", [lists:sum(MidEls)]),

    Fixed = [fix_invalid(lists:reverse(I), PageOrderRules) || I <- Invalid],

    MidFixedEls = [lists:nth(Len div 2 + 1, V) || V <- Fixed, Len <- [length(V)]],
    io:format("fixed sum: ~w~n", [lists:sum(MidFixedEls)]).

parse_input(Input) ->
    Rows = binary:split(Input, <<"\n">>, ['global', 'trim']),
    {PageOrderRules, PageUpdates} = lists:splitwith(fun(Row) -> Row =/= <<>> end, Rows),
    {maps:groups_from_list(fun({X, _}) -> X end
                          ,[list_to_tuple([binary_to_integer(Bin) || Bin <- binary:split(Rule, <<"|">>)])
                            || Rule <- PageOrderRules
                           ])
    ,[[binary_to_integer(Bin) || Bin <- binary:split(Update, <<",">>, ['global'])]
      || Update <- PageUpdates,
         Update =/= <<>>
     ]
    }.

fix_invalid(PageUpdate, PageOrderRules) ->
    lists:usort(fun(Page1, Page2) -> sort_invalid(Page1, Page2, PageOrderRules) end, PageUpdate).

sort_invalid(Page1, Page2, PageOrderRules) ->
    case maps:get(Page1, PageOrderRules, 'undefined') of
        'undefined' -> 'true'; % if no Page1 update, it should go after Page 2
        Page1Rules ->
            %% if page2 is not in page1's order rules, page1 goes after
            'false' =:= lists:keyfind(Page2, 2, Page1Rules)
    end.

is_valid_update([_Page], _) -> 'true';
is_valid_update([Page | PageUpdate], PageOrderRules) ->
    %% Page is head of list, so all PageUpdate must come after it in Rules
    case maps:get(Page, PageOrderRules, 'undefined') of
        'undefined' ->
            'false'; % skip update
        Rules ->
            %% all PU must be 2nd el in Rules
            case lists:all(fun(PU) ->
                                   lists:keyfind(PU, 2, Rules) =/= 'false'
                           end
                          ,PageUpdate
                          )
            of
                'true' -> is_valid_update(PageUpdate, PageOrderRules);
                'false' ->
                    'false'
            end
    end.
