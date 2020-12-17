#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% Now that you've identified which tickets contain invalid values,
%% discard those tickets entirely. Use the remaining valid tickets to
%% determine which field is which.

%% Using the valid ranges for each field, determine what order the
%% fields appear on the tickets. The order is consistent between all
%% tickets: if seat is the third field, it is the third field on every
%% ticket, including your ticket.

%% For example, suppose you have the following notes:

%% class: 0-1 or 4-19
%% row: 0-5 or 8-19
%% seat: 0-13 or 16-19

%% your ticket:
%% 11,12,13

%% nearby tickets:
%% 3,9,18
%% 15,1,5
%% 5,14,9

%% Based on the nearby tickets in the above example, the first position
%% must be row, the second position must be class, and the third
%% position must be seat; you can conclude that in your ticket, class
%% is 12, row is 11, and seat is 13.

%% Once you work out which field is which, look for the six fields on
%% your ticket that start with the word departure. What do you get if
%% you multiply those six values together?

main(_) ->
    #{nearby := Nearby, rules := Rules, mine := Mine} = read_input("p16.txt"),
    ValidTickets = filter_invalid_tickets(Rules, Nearby, []),
    PositionLabels = decode_tickets(Rules, ValidTickets), %% {position, label}
    Departures = fields_with_departure(PositionLabels, Mine),
    %% Product = lists:foldl(fun erlang:'*'/2, 1, Departures),
    io:format("product: ~p~n", [lists:foldl(fun erlang:'*'/2, 1, Departures)]).

fields_with_departure(Positions, Ticket) ->
    fields_with_departure(Positions, Ticket, []).

fields_with_departure([], [], Acc) -> Acc;
fields_with_departure([{_, <<"departure", _/binary>>} | Positions], [TValue | Ticket], Acc) ->
    fields_with_departure(Positions, Ticket, [TValue | Acc]);
fields_with_departure([{_, _} | Positions], [_TValue | Ticket], Acc) ->
    fields_with_departure(Positions, Ticket, Acc).

decode_tickets(Rules, ValidTickets) ->
    decode_tickets(Rules, ValidTickets, #{}).

%% Rule = {label, Ranges}
%% RuleLocation = #{TicketIndex => [label]}
decode_tickets(Rules, ValidTickets, RuleLocations) ->
    {_, Locations} = lists:foldl(fun decode_ticket/2, {Rules, RuleLocations}, ValidTickets),
    filter_false_locations(Locations).

filter_false_locations(Locations) ->
    Ls = maps:fold(fun filter_false_labels/3, #{}, Locations),
    resolve_label_locations(Ls, []).

resolve_label_locations(Locations, Resolved) ->
    case maps:fold(fun resolve_label_location/3
                  ,{Locations, Resolved, 0}
                  ,Locations
                  )
    of
        {_Ls, Rs, 0} -> lists:keysort(1, Rs); % fully resolved
        {Ls, Rs, _Res} ->
            resolve_label_locations(Ls, Rs)
    end.
resolve_label_location(Index, [{Label, 'true'}], {Locations, Resolved, Resolutions}) ->
    %% remove Label from all other indices
    {maps:fold(fun(I,Ls,Acc) -> remove_label(I,Ls,Acc, Index, Label) end
              ,Locations
              ,Locations
              )
    ,[{Index, Label} | Resolved]
    ,Resolutions+1
    };
resolve_label_location(_Index, _Labels, Acc) ->
    Acc.

remove_label(Index, _, Acc, Index, _) -> maps:remove(Index, Acc);
remove_label(Index, Labels, Acc, _, Label) -> maps:put(Index, lists:keydelete(Label,1, Labels), Acc).

filter_false_labels(Index, Labels, Locations) ->
    maps:put(Index
            ,maps:to_list(maps:filter(fun(_, IsValid) -> IsValid end, Labels))
            ,Locations
            ).


%% Rule = {label, Ranges}
%% RuleLocation = {TicketIndex, label}
decode_ticket(Ticket, {Rules, Locations}) ->
    {_I, _, Ls} = lists:foldl(fun assoc_tvalue/2, {1, Rules, Locations}, Ticket),
    {Rules, Ls}.

assoc_tvalue(TValue, {Index, Rules, Locations}) ->
    {Index+1, Rules, lists:foldl(fun(R, L) -> assoc_rule(R, L, Index, TValue) end, Locations, Rules)}.

assoc_rule({Label, Ranges}, Locations, Index, TValue) ->
    case maps:get(Index, Locations, #{}) of
        #{Label := 'false'} -> Locations; % previous ticket[Index] says no
        Labels ->
            IsValid = is_valid_value1(TValue, Ranges),
            maps:put(Index, Labels#{Label => IsValid}, Locations)
    end.

filter_invalid_tickets(_Rules, [], ValidTickets) ->
    ValidTickets;
filter_invalid_tickets(Rules, [Ticket | Tickets], Valid) ->
    case is_ticket_valid(Rules, Ticket) of
        'true' -> filter_invalid_tickets(Rules, Tickets, [Ticket | Valid]);
        'false' -> filter_invalid_tickets(Rules, Tickets, Valid)
    end.

is_ticket_valid(Rules, Ticket) ->
    lists:all(fun(TValue) -> is_valid_value(TValue, Rules) end
             ,Ticket
             ).

is_valid_value(TValue, Rules) ->
    lists:any(fun(Rule) -> is_valid_value1(TValue, Rule) end
             ,Rules
             ).

is_valid_value1(TValue, {_Label, Ranges}) ->
    is_valid_value1(TValue, Ranges);
is_valid_value1(_TValue, []) -> 'false';
is_valid_value1(TValue, [{Min, Max} | Ranges]) ->
    case Min =< TValue andalso TValue =< Max of
        'true' -> 'true';
        'false' -> is_valid_value1(TValue, Ranges)
    end.

read_input(Filename) ->
    {'ok', File} = file:read_file(Filename),
    {_, TicketInfo} = lists:foldl(fun parse_data/2
                                 ,{'rule', #{rules => [], nearby => []}}
                                 ,binary:split(File, <<"\n">>, ['global', 'trim'])
                                 ),
    TicketInfo.

parse_data(<<>>, Acc) -> Acc;
parse_data(<<"your ticket:">>, {_, Acc}) ->
    {'mine', Acc};
parse_data(<<"nearby tickets:">>, {_, Acc}) ->
    {'nearby', Acc};
parse_data(Line, {'rule', #{rules := Rules}=Acc}) ->
    Rule = parse_rule(Line),
    {'rule', Acc#{rules => [Rule | Rules]}};
parse_data(Line, {'mine', Acc}) ->
    Ticket = parse_ticket(Line),
    {'mine', Acc#{mine => Ticket}};
parse_data(Line, {'nearby', #{nearby := Nearby}=Acc}) ->
    Ticket = parse_ticket(Line),
    {'nearby', Acc#{nearby => [Ticket | Nearby]}}.

parse_rule(Line) ->
    [Label, Rules] = binary:split(Line, <<": ">>),
    {'match', Ranges} = re:run(Rules, <<"([\\d]+)">>, [{'capture', 'all_but_first', 'binary'}, 'global']),
    {Label, parse_ranges(Ranges, [])}.

parse_ranges([], Acc) ->
    lists:reverse(Acc);
parse_ranges([[Min], [Max] | Ranges], Acc) ->
    parse_ranges(Ranges, [{binary_to_integer(Min, 10), binary_to_integer(Max, 10)} | Acc]).

parse_ticket(Line) ->
    [binary_to_integer(Bin, 10) || Bin <- binary:split(Line, <<",">>, ['global'])].
