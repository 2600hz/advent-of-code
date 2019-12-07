-module(intcode).

%% Day 2 helpers
-export([from_binary/1
        ,to_binary/1
        ,run/1
        ,set_output_fun/2
        ,set_input_fun/2
        ]).

-define(INTCODE_HALT, 99).
-define(INTCODE_SUM, 1).
-define(INTCODE_MULT, 2).
-define(INTCODE_INPUT, 3).
-define(INTCODE_OUTPUT, 4).
-define(INTCODE_IF_TRUE, 5).
-define(INTCODE_IF_FALSE, 6).
-define(INTCODE_LESS_THAN, 7).
-define(INTCODE_EQUAL_TO, 8).

-define(MODE_POSITION, 0).
-define(MODE_IMMEDIATE, 1).

-type output_fun() :: fun((any(), intcode_program()) -> 'ok').
-type input_fun() :: fun(() -> 0..9).

-type intcode_program() :: #{'program' => #{non_neg_integer() => integer()}
                            ,'output_fun' => output_fun()
                            ,'input_fun' => input_fun()
                            }.

%% should take <<"a,b,c">> and create #{0=>a,1=>b,2=>c}
-spec from_binary(binary()) -> intcode_program().
from_binary(Contents) ->
    Opcodes = binary:split(binary:replace(Contents, <<"\n">>, <<>>), <<",">>, ['global']),
    Count = length(Opcodes),
    #{'program' => maps:from_list(lists:zip(lists:seq(0, Count-1), lists:map(fun binary_to_integer/1, Opcodes)))
     ,'output_fun' => fun default_output_fun/2
     ,'input_fun' => fun default_input_fun/0
     }.

default_output_fun(Value, _Intcode) -> io:format('standard_io', "~p~n", [Value]).

default_input_fun() ->
    InputChar = io:get_chars("enter system ID: ", 1),
    list_to_integer(InputChar, 10).

-spec to_binary(intcode_program()) -> binary().
to_binary(#{'program' := Intcode}) ->
    {_Indicies, Opcodes} = lists:unzip(lists:keysort(1, maps:to_list(Intcode))),
    list_to_binary(lists:join(<<",">>, [integer_to_binary(I) || I <- Opcodes])).

-spec set_output_fun(intcode_program(), output_fun()) -> intcode_program().
set_output_fun(Intcode, OutputFun) ->
    Intcode#{'output_fun' => OutputFun}.

-spec set_input_fun(intcode_program(), input_fun()) -> intcode_program().
set_input_fun(Intcode, InputFun) ->
    Intcode#{'input_fun' => InputFun}.

-spec run(intcode_program()) -> integer().
run(Intcode) ->
    run(0, Intcode).

-spec run(non_neg_integer(), intcode_program()) -> integer().
run(InstructionPointer, Intcode) ->
    Opcode = opcode(InstructionPointer, Intcode),
    process_opcode(InstructionPointer, Intcode, Opcode).

-spec opcode(non_neg_integer(), intcode_program()) ->
          {pos_integer(), [0|1]}.
opcode(InstructionPointer, Intcode) ->
    %% io:format("  ~p: ", [InstructionPointer]),
    OpcodePlusParameterModes = parameter_value(InstructionPointer, ?MODE_POSITION, Intcode),
    decode_opcode(integer_to_list(OpcodePlusParameterModes)).

decode_opcode([_]=LegacyOpcode) ->
    Opcode = list_to_integer(LegacyOpcode, 10),
    Modes = opcode_parameter_modes(Opcode, []),
    %% io:format('standard_io', "LOP ~5.s -> ~6.s modes: ~p~n", [LegacyOpcode, pp_opcode(Opcode), Modes]),
    {Opcode, Modes};
decode_opcode(OpcodePlusParameterModes) ->
    [Bottom, Top | ParameterModes] = lists:reverse(OpcodePlusParameterModes),
    Opcode = list_to_integer([Top, Bottom], 10),
    Modes = opcode_parameter_modes(Opcode, ParameterModes),
    %% io:format('standard_io', "OP ~5.s -> ~6.s modes: ~p~n", [OpcodePlusParameterModes, pp_opcode(Opcode), Modes]),
    {Opcode, Modes}.

pp_opcode(?INTCODE_SUM)    -> <<"sum">>;
pp_opcode(?INTCODE_MULT)   -> <<"mult">>;
pp_opcode(?INTCODE_INPUT)  -> <<"input">>;
pp_opcode(?INTCODE_OUTPUT) -> <<"output">>;
pp_opcode(?INTCODE_HALT)   -> <<"halt">>;
pp_opcode(?INTCODE_IF_TRUE) -> <<"is_true">>;
pp_opcode(?INTCODE_IF_FALSE) -> <<"is_false">>;
pp_opcode(?INTCODE_LESS_THAN) -> <<"less_than">>;
pp_opcode(?INTCODE_EQUAL_TO) -> <<"equals">>.

%% ParameterModes = [$0 | $1] in right->left order
%% pads end of list with $0 to get full length of parameters for opcode
opcode_parameter_modes(?INTCODE_HALT, []) -> [];
opcode_parameter_modes(?INTCODE_SUM, ParameterModes) ->
    parameter_modes(ParameterModes, 3);
opcode_parameter_modes(?INTCODE_MULT, ParameterModes) ->
    parameter_modes(ParameterModes, 3);
opcode_parameter_modes(?INTCODE_INPUT, ParameterModes) ->
    parameter_modes(ParameterModes, 1);
opcode_parameter_modes(?INTCODE_OUTPUT, ParameterModes) ->
    parameter_modes(ParameterModes, 1);
opcode_parameter_modes(?INTCODE_IF_TRUE, ParameterModes) ->
    parameter_modes(ParameterModes, 2);
opcode_parameter_modes(?INTCODE_IF_FALSE, ParameterModes) ->
    parameter_modes(ParameterModes, 2);
opcode_parameter_modes(?INTCODE_LESS_THAN, ParameterModes) ->
    parameter_modes(ParameterModes, 3);
opcode_parameter_modes(?INTCODE_EQUAL_TO, ParameterModes) ->
    parameter_modes(ParameterModes, 3).

parameter_modes(ParameterModes, Parameters) ->
    [Char-$0 || Char <- lists:flatten(string:pad(ParameterModes, Parameters, 'trailing', $0))].

parameter(InstructionPointer, Position, #{'program' := Intcode}) ->
    maps:get(InstructionPointer+Position, Intcode).

parameter_value(Parameter, ?MODE_POSITION, #{'program' := Intcode}) ->
    Value = maps:get(Parameter, Intcode),
    %% io:format('standard_io', "    looking up value at position ~p: ~p~n", [Parameter, Value]),
    Value;
parameter_value(Parameter, ?MODE_IMMEDIATE, _Intcode) ->
    %% io:format('standard_io', "    using ~p as value in immediate mode~n", [Parameter]),
    Parameter.

process_opcode(_InstructionPointer, Intcode, {?INTCODE_HALT, _Modes}) ->
    %% io:format('standard_io', "halting at ~p~n", [intcode_to_binary(Intcode)]),
    parameter_value(0, ?MODE_POSITION, Intcode);
process_opcode(InstructionPointer, Intcode, {?INTCODE_SUM, ParameterModes}) ->
    sum(InstructionPointer, Intcode, ParameterModes);
process_opcode(InstructionPointer, Intcode, {?INTCODE_MULT, ParameterModes}) ->
    multiply(InstructionPointer, Intcode, ParameterModes);
process_opcode(InstructionPointer, Intcode, {?INTCODE_INPUT, ParameterModes}) ->
    input(InstructionPointer, Intcode, ParameterModes);
process_opcode(InstructionPointer, Intcode, {?INTCODE_OUTPUT, ParameterModes}) ->
    output(InstructionPointer, Intcode, ParameterModes);
process_opcode(InstructionPointer, Intcode, {?INTCODE_IF_TRUE, ParameterModes}) ->
    if_true(InstructionPointer, Intcode, ParameterModes);
process_opcode(InstructionPointer, Intcode, {?INTCODE_IF_FALSE, ParameterModes}) ->
    if_false(InstructionPointer, Intcode, ParameterModes);
process_opcode(InstructionPointer, Intcode, {?INTCODE_LESS_THAN, ParameterModes}) ->
    less_than(InstructionPointer, Intcode, ParameterModes);
process_opcode(InstructionPointer, Intcode, {?INTCODE_EQUAL_TO, ParameterModes}) ->
    equal_to(InstructionPointer, Intcode, ParameterModes).

step(InstructionPointer, Parameters, Intcode) ->
    run(InstructionPointer+Parameters, Intcode).

sum(InstructionPointer, Intcode, ParameterModes) ->
    NewIntcode = process_instruction(InstructionPointer, Intcode, ParameterModes, fun erlang:'+'/2),
    step(InstructionPointer, 4, NewIntcode).

multiply(InstructionPointer, Intcode, ParameterModes) ->
    NewIntcode = process_instruction(InstructionPointer, Intcode, ParameterModes, fun erlang:'*'/2),
    step(InstructionPointer, 4, NewIntcode).

input(InstructionPointer, #{'input_fun' := InputFun}=Intcode, [_ParameterMode]) ->
    StoragePointer = parameter(InstructionPointer, 1, Intcode),
    InputValue = InputFun(),
    NewIntcode = store(StoragePointer, InputValue, Intcode),
    %% io:format('standard_io', "    put ~p to ~p~n", [InputChars, StoragePointer]),
    step(InstructionPointer, 2, NewIntcode).

output(InstructionPointer, #{'output_fun' := OutputFun}=Intcode, [_ParameterMode]) ->
    StoragePointer = parameter(InstructionPointer, 1, Intcode),
    Value = parameter_value(StoragePointer, ?MODE_POSITION, Intcode),
    OutputFun(Value, Intcode),
    step(InstructionPointer, 2, Intcode).

process_instruction(InstructionPointer, Intcode, [FirstMode, SecondMode, _ThirdMode], Applier) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    SecondParameter = parameter(InstructionPointer, 2, Intcode),
    StoragePointer = parameter(InstructionPointer, 3, Intcode),

    FirstOperand = parameter_value(FirstParameter, FirstMode, Intcode),
    SecondOperand = parameter_value(SecondParameter, SecondMode, Intcode),
    Result = Applier(FirstOperand, SecondOperand),

    %% io:format('standard_io'
    %%          ,"    store ~4.w: ~8.w (~p ~8.w(~5.w) ~8.w(~5.w))~n"
    %%          ,[StoragePointer, Result, Applier, FirstOperand, FirstParameter, SecondOperand, SecondParameter]
    %%          ),
    store(StoragePointer, Result, Intcode).

store(StoragePointer, Value, #{'program' := Program}=Intcode) ->
    Intcode#{'program' => maps:put(StoragePointer, Value, Program)}.

if_true(InstructionPointer, Intcode, [FirstMode, SecondMode]) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    case parameter_value(FirstParameter, FirstMode, Intcode) of
        0 -> step(InstructionPointer, 3, Intcode);
        _NonZero ->
            SecondParameter = parameter(InstructionPointer, 2, Intcode),
            JumpTo = parameter_value(SecondParameter, SecondMode, Intcode),
            step(JumpTo, 0, Intcode)
    end.

if_false(InstructionPointer, Intcode, [FirstMode, SecondMode]) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    case parameter_value(FirstParameter, FirstMode, Intcode) of
        0 ->
            SecondParameter = parameter(InstructionPointer, 2, Intcode),
            JumpTo = parameter_value(SecondParameter, SecondMode, Intcode),
            step(JumpTo, 0, Intcode);
        _NonZero ->
            step(InstructionPointer, 3, Intcode)
    end.

less_than(InstructionPointer, Intcode, [FirstMode, SecondMode, _ThirdMode]) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    SecondParameter = parameter(InstructionPointer, 2, Intcode),
    StoragePosition = parameter(InstructionPointer, 3, Intcode),

    NewIntcode =
        case parameter_value(FirstParameter, FirstMode, Intcode)
            < parameter_value(SecondParameter, SecondMode, Intcode)
        of
            'true' ->
                store(StoragePosition, 1, Intcode);
            'false' ->
                store(StoragePosition, 0, Intcode)
        end,
    step(InstructionPointer, 4, NewIntcode).

equal_to(InstructionPointer, Intcode, [FirstMode, SecondMode, _ThirdMode]) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    SecondParameter = parameter(InstructionPointer, 2, Intcode),
    StoragePosition = parameter(InstructionPointer, 3, Intcode),

    NewIntcode =
        case parameter_value(FirstParameter, FirstMode, Intcode)
            == parameter_value(SecondParameter, SecondMode, Intcode)
        of
            'true' ->
                store(StoragePosition, 1, Intcode);
            'false' ->
                store(StoragePosition, 0, Intcode)
        end,
    step(InstructionPointer, 4, NewIntcode).
