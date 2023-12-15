-module(day15).

-export([hash_sum/1
        ,focusing_power/1
        ]).

hash_sum(Input) ->
    Steps = parse_steps(Input, 1),
    lists:sum([hash(Step) || Step <- Steps]).

focusing_power(Input) ->
    Steps = parse_steps(Input, 2),
    Steps1 = [add_hash(Step) || Step <- Steps],
    Boxes = initialize(Steps1, array:new(256, {'default', []})),
    boxes_to_focusing_power(Boxes).

boxes_to_focusing_power(Boxes) ->
    {_Slot, FocusingPower} = array:foldl(fun focusing_power/3, {0, 0}, Boxes),
    FocusingPower.

focusing_power(_Hash, [], {_Slot, FocusingPower}) -> {0, FocusingPower};
focusing_power(Hash, [{_Label, FocalLength} | Lenses], {Slot, FocusingPower}) ->
    Slot1 = Slot + 1,
    FocusingPower1 = FocusingPower + (Hash + 1) * Slot1 * FocalLength,
    focusing_power(Hash, Lenses, {Slot1, FocusingPower1}).

initialize([], Boxes) ->
    array:map(fun(_Hash, Lenses) -> Lenses end, Boxes);
initialize([{'add_lens', Label, FocalLength, Hash} | Steps], Boxes) ->
    Lenses = array:get(Hash, Boxes),
    Lenses1 = lists:keystore(Label, 1, Lenses, {Label, FocalLength}),
    initialize(Steps, array:set(Hash, Lenses1, Boxes));
initialize([{'remove_lens', Label, Hash} | Steps], Boxes) ->
    Lenses = array:get(Hash, Boxes),
    Lenses1 = lists:keydelete(Label, 1, Lenses),
    initialize(Steps, array:set(Hash, Lenses1, Boxes)).

add_hash({'add_lens', Label, FocalLength}) ->
    {'add_lens', Label, FocalLength, hash(Label)};
add_hash({'remove_lens', Label}) ->
    {'remove_lens', Label, hash(Label)}.

hash(Step) -> hash(Step, 0).

hash(<<>>, Hash) -> Hash;
hash(<<C:8, Rest/binary>>, Hash) -> hash(Rest, (Hash + C) * 17 rem 256).

parse_steps(Input, 1) -> binary:split(Input, <<",">>, ['global']);
parse_steps(Input, 2) ->
    Steps = parse_steps(Input, 1),
    [parse_step(Step) || Step <- Steps].

parse_step(Step) -> parse_step(Step, <<>>).

parse_step(<<"-">>, Label) -> {'remove_lens', Label};
parse_step(<<"=", FocalLength/binary>>, Label) ->
    {'add_lens', Label, binary_to_integer(FocalLength)};
parse_step(<<C:8, Rest/binary>>, Label) ->
    parse_step(Rest, <<Label/binary, C>>).
