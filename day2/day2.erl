-module(day2).
-export([test_new_computer/0, find_input_pair_checksum/0]).

read_input(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<",">>], [global]).

process_input(L) ->
    [ list_to_integer(binary_to_list(X)) || X <- lists:append(lists:sublist(L, length(L) - 1), [binary:replace(lists:nth(length(L), L), <<"\n">>, <<"">>)]) ].

replace_value(L, I, X) ->
    Head = lists:sublist(L, 1, I - 1),
    Tail = lists:sublist(L, I + 1, length(L)),
    Head ++ [X] ++ Tail.

change_noun_and_verb(X, Y) ->
    Input = process_input(read_input("day2.input")),
    InputReplaceOne = replace_value(Input, 1 + 1, X),
    replace_value(InputReplaceOne, 2 + 1, Y).

execute_intcode(L) ->
    execute_intcode(L, 1).

execute_intcode(L, I) ->
    Opcode = lists:sublist(L, I, 4),
    [Operation, Position1, Position2, PosToReplace] = Opcode,
    Value1 = lists:nth(Position1 + 1, L),
    Value2 = lists:nth(Position2 + 1, L),
    case Operation of
        1 ->
            execute_intcode(replace_value(L, PosToReplace + 1, Value1 + Value2), I + 4);
        2 ->
            execute_intcode(replace_value(L, PosToReplace + 1, Value1 * Value2), I + 4);
        99 ->
            L;
        _ -> io:format("System Malfunction!")
    end.

test_input_pair(Noun, Verb) ->
    Input = change_noun_and_verb(Noun, Verb),
    lists:nth(1, execute_intcode(Input)).

find_input_pair(Output, N) ->
    [ {Noun, Verb} ||
        Noun <- lists:seq(1, N),
        Verb <- lists:seq(1, N),
        test_input_pair(Noun, Verb) =:= Output
    ].

test_new_computer() ->
    Input = change_noun_and_verb(12, 2),
    lists:nth(1, execute_intcode(Input)).

find_input_pair_checksum() ->
    InputPair = find_input_pair(19690720, 99),
    [{Noun, Verb}] = InputPair,
    Checksum = (100 * Noun) + Verb,
    io:format("Noun = ~p~nVerb = ~p~nChecksum = ~p~n", [Noun, Verb, Checksum]).

