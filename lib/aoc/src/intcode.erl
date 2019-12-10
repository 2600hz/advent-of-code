-module(intcode).

-export([from_binary/1
        ,to_binary/1
        ,run/1
        ,set_output_fun/2
        ,set_input_fun/2
        ]).

-export([tests/0]).

-define(INTCODE_HALT, 99).
-define(INTCODE_SUM, 1).
-define(INTCODE_MULT, 2).
-define(INTCODE_INPUT, 3).
-define(INTCODE_OUTPUT, 4).
-define(INTCODE_IF_TRUE, 5).
-define(INTCODE_IF_FALSE, 6).
-define(INTCODE_LESS_THAN, 7).
-define(INTCODE_EQUAL_TO, 8).
-define(INTCODE_RELATIVE_BASE, 9).

-define(MODE_POSITION, 0).
-define(MODE_IMMEDIATE, 1).
-define(MODE_RELATIVE, 2).

-type output_fun() :: fun((any(), intcode_program()) -> 'ok').
-type input_fun() :: fun(() -> 0..9).

-type intcode_program() :: #{'program' => #{non_neg_integer() => integer()}
                            ,'instruction_pointer' => non_neg_integer()
                            ,'relative_base' => non_neg_integer()
                            ,'output_fun' => output_fun()
                            ,'input_fun' => input_fun()
                            }.

%% should take <<"a,b,c">> and create #{0=>a,1=>b,2=>c}
-spec from_binary(binary()) -> intcode_program().
from_binary(Contents) ->
    Opcodes = binary:split(binary:replace(Contents, <<"\n">>, <<>>), <<",">>, ['global']),
    Count = length(Opcodes),
    #{'program' => maps:from_list(lists:zip(lists:seq(0, Count-1), lists:map(fun binary_to_integer/1, Opcodes)))
     ,'instruction_pointer' => 0
     ,'relative_base' => 0
     ,'output_fun' => fun default_output_fun/2
     ,'input_fun' => fun default_input_fun/0
     }.

default_output_fun(Value, _Intcode) -> io:format('standard_io', "<<< ~p~n", [Value]).

default_input_fun() ->
    InputChar = io:get_chars(">>> ", 1),
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
    Opcode = opcode(Intcode),
    process_opcode(Intcode, Opcode).

-spec opcode(intcode_program()) ->
          {pos_integer(), [0|1]}.
opcode(#{'instruction_pointer' := InstructionPointer}=Intcode) ->
    OpcodePlusParameterModes = parameter_value(InstructionPointer, ?MODE_POSITION, Intcode),
    decode_opcode(InstructionPointer, integer_to_list(OpcodePlusParameterModes)).

decode_opcode(_IP, [_]=LegacyOpcode) ->
    Opcode = list_to_integer(LegacyOpcode, 10),
    Modes = opcode_parameter_modes(Opcode, []),
    %%  io:format('standard_io', "LOP ~6.s@~p -> ~9.s modes: ~p~n", [LegacyOpcode, IP, pp_opcode(Opcode), Modes]),
    {Opcode, Modes};
decode_opcode(_IP, OpcodePlusParameterModes) ->
    [Bottom, Top | ParameterModes] = lists:reverse(OpcodePlusParameterModes),
    Opcode = list_to_integer([Top, Bottom], 10),
    Modes = opcode_parameter_modes(Opcode, ParameterModes),
    %% io:format('standard_io', "OP ~6.s@~p -> ~9.s modes: ~p~n", [OpcodePlusParameterModes, IP, pp_opcode(Opcode), Modes]),
    {Opcode, Modes}.

pp_opcode(?INTCODE_SUM)    -> <<"sum">>;
pp_opcode(?INTCODE_MULT)   -> <<"mult">>;
pp_opcode(?INTCODE_INPUT)  -> <<"input">>;
pp_opcode(?INTCODE_OUTPUT) -> <<"output">>;
pp_opcode(?INTCODE_HALT)   -> <<"halt">>;
pp_opcode(?INTCODE_IF_TRUE) -> <<"if_true">>;
pp_opcode(?INTCODE_IF_FALSE) -> <<"if_false">>;
pp_opcode(?INTCODE_LESS_THAN) -> <<"less_than">>;
pp_opcode(?INTCODE_EQUAL_TO) -> <<"equals">>;
pp_opcode(?INTCODE_RELATIVE_BASE) -> <<"relative">>.

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
    parameter_modes(ParameterModes, 3);
opcode_parameter_modes(?INTCODE_RELATIVE_BASE, ParameterModes) ->
    parameter_modes(ParameterModes, 1).

parameter_modes(ParameterModes, Parameters) ->
    [Char-$0 || Char <- lists:flatten(string:pad(ParameterModes, Parameters, 'trailing', $0))].

parameter(Pointer, RelativePosition, #{'program' := Intcode}) ->
    maps:get(Pointer+RelativePosition, Intcode).

parameter_value(Parameter, ?MODE_POSITION, #{'program' := Intcode}) when Parameter >= 0 ->
    Value = maps:get(Parameter, Intcode, 0),
    %%  io:format('standard_io', "    mode:pos looking up value at position ~p: ~p~n", [Parameter, Value]),
    Value;
parameter_value(Parameter, ?MODE_IMMEDIATE, _Intcode) ->
    %%  io:format('standard_io', "    mode:imm using ~p as value in immediate mode~n", [Parameter]),
    Parameter;
parameter_value(Parameter, ?MODE_RELATIVE, #{'relative_base' := Base}=Intcode) ->
    Position = Parameter + Base,
    %%  io:format("    mode:rel adjusting position ~p by ~p: ~p~n", [Parameter, Base, Position]),
    parameter_value(Position, ?MODE_POSITION, Intcode).

process_opcode(Intcode, {?INTCODE_HALT, _Modes}) ->
    parameter_value(0, ?MODE_POSITION, Intcode);
process_opcode(Intcode, {?INTCODE_SUM, ParameterModes}) ->
    sum(Intcode, ParameterModes);
process_opcode(Intcode, {?INTCODE_MULT, ParameterModes}) ->
    multiply(Intcode, ParameterModes);
process_opcode(Intcode, {?INTCODE_INPUT, ParameterModes}) ->
    input(Intcode, ParameterModes);
process_opcode(Intcode, {?INTCODE_OUTPUT, ParameterModes}) ->
    output(Intcode, ParameterModes);
process_opcode(Intcode, {?INTCODE_IF_TRUE, ParameterModes}) ->
    if_true(Intcode, ParameterModes);
process_opcode(Intcode, {?INTCODE_IF_FALSE, ParameterModes}) ->
    if_false(Intcode, ParameterModes);
process_opcode(Intcode, {?INTCODE_LESS_THAN, ParameterModes}) ->
    less_than(Intcode, ParameterModes);
process_opcode(Intcode, {?INTCODE_EQUAL_TO, ParameterModes}) ->
    equal_to(Intcode, ParameterModes);
process_opcode(Intcode, {?INTCODE_RELATIVE_BASE, ParameterModes}) ->
    relative_base(Intcode, ParameterModes).

step(Parameters, #{'instruction_pointer' := InstructionPointer}=Intcode) ->
    run(Intcode#{'instruction_pointer' => InstructionPointer+Parameters}).

%% Opcode 1 adds together numbers read from two positions and stores
%% the result in a third position. The three integers immediately
%% after the opcode tell you these three positions - the first two
%% indicate the positions from which you should read the input values,
%% and the third indicates the position at which the output should be
%% stored.
%% For example, if your Intcode computer encounters 1,10,20,30, it
%% should read the values at positions 10 and 20, add those values,
%% and then overwrite the value at position 30 with their sum.
sum(Intcode, ParameterModes) ->
    NewIntcode = process_instruction(Intcode, ParameterModes, fun erlang:'+'/2),
    step(4, NewIntcode).

%% Opcode 2 works exactly like opcode 1, except it multiplies the two
%% inputs instead of adding them. Again, the three integers after the
%% opcode indicate where the inputs and outputs are, not their values.
multiply(Intcode, ParameterModes) ->
    NewIntcode = process_instruction(Intcode, ParameterModes, fun erlang:'*'/2),
    step(4, NewIntcode).

%% Opcode 3 takes a single integer as input and saves it to the
%% position given by its only parameter. For example, the instruction
%% 3,50 would take an input value and store it at address 50.
input(#{'instruction_pointer' := InstructionPointer
       ,'input_fun' := InputFun
       }=Intcode
     ,[ParameterMode]
     ) ->
    Parameter = parameter(InstructionPointer, 1, Intcode),
    StoragePointer = storage_pointer(Parameter, ParameterMode, Intcode),
    %%  io:format("    reading input and storing at ~p(~p:~p)~n", [StoragePointer, Parameter, ParameterMode]),

    InputValue = InputFun(),
    NewIntcode = store(StoragePointer, InputValue, Intcode),
    step(2, NewIntcode).

storage_pointer(Parameter, ?MODE_RELATIVE, #{'relative_base' := Base}) ->
    Parameter + Base;
storage_pointer(Parameter, _Mode, _Intcode) ->
    Parameter.

%% Opcode 4 outputs the value of its only parameter. For example, the
%% instruction 4,50 would output the value at address 50.
output(#{'instruction_pointer' := InstructionPointer
        ,'output_fun' := OutputFun
        }=Intcode
      ,[ParameterMode]
      ) ->
    StoragePointer = parameter(InstructionPointer, 1, Intcode),

    Value = parameter_value(StoragePointer, ParameterMode, Intcode),

    %%  io:format("  outputting from ~p:~p: ~p~n", [StoragePointer, ParameterMode, Value]),

    OutputFun(Value, Intcode),
    step(2, Intcode).

process_instruction(#{'instruction_pointer' := InstructionPointer}=Intcode, [FirstMode, SecondMode, ThirdMode], Applier) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    SecondParameter = parameter(InstructionPointer, 2, Intcode),
    ThirdParameter = parameter(InstructionPointer, 3, Intcode),
    StoragePointer = storage_pointer(ThirdParameter, ThirdMode, Intcode),

    FirstOperand = parameter_value(FirstParameter, FirstMode, Intcode),
    SecondOperand = parameter_value(SecondParameter, SecondMode, Intcode),
    Result = Applier(FirstOperand, SecondOperand),

    %% %%  io:format('standard_io'
    %%          ,"  store ~4.w: ~w (~p ~w(~w) ~w(~w))~n"
    %%          ,[StoragePointer, Result, Applier, FirstOperand, FirstParameter, SecondOperand, SecondParameter]
    %%          ),
    store(StoragePointer, Result, Intcode).

%% Parameters that an instruction writes to will never be in immediate
%% mode.
store(StoragePointer, Value, #{'program' := Program}=Intcode) ->
    case maps:get(StoragePointer, Program, 'undefined') of
        'undefined' -> 'ok';
        %%  io:format("    store in ~p: ~p~n", [StoragePointer, Value]);
        _Old -> 'ok'
                %%  io:format("    ovwrt in ~p: ~p (~p)~n", [StoragePointer, Value, _Old])
    end,
    Intcode#{'program' => maps:put(StoragePointer, Value, Program)}.

%% Opcode 5 is jump-if-true: if the first parameter is non-zero, it
%% sets the instruction pointer to the value from the second
%% parameter. Otherwise, it does nothing.
if_true(#{'instruction_pointer' := InstructionPointer}=Intcode, [FirstMode, SecondMode]) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    case parameter_value(FirstParameter, FirstMode, Intcode) of
        0 -> step(3, Intcode);
        _NonZero ->
            SecondParameter = parameter(InstructionPointer, 2, Intcode),
            JumpTo = parameter_value(SecondParameter, SecondMode, Intcode),
            %%  io:format("    jumping to ~p~n", [JumpTo]),
            step(0, Intcode#{'instruction_pointer' := JumpTo})
    end.

%% Opcode 6 is jump-if-false: if the first parameter is zero, it sets
%% the instruction pointer to the value from the second
%% parameter. Otherwise, it does nothing.
if_false(#{'instruction_pointer' := InstructionPointer}=Intcode, [FirstMode, SecondMode]) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    case parameter_value(FirstParameter, FirstMode, Intcode) of
        0 ->
            SecondParameter = parameter(InstructionPointer, 2, Intcode),
            JumpTo = parameter_value(SecondParameter, SecondMode, Intcode),
            %%  io:format("    jumping to ~p~n", [JumpTo]),
            step(0, Intcode#{'instruction_pointer' := JumpTo});
        _NonZero ->
            step(3, Intcode)
    end.

%% Opcode 7 is less than: if the first parameter is less than the
%% second parameter, it stores 1 in the position given by the third
%% parameter. Otherwise, it stores 0.
less_than(#{'instruction_pointer' := InstructionPointer}=Intcode, [FirstMode, SecondMode, ThirdMode]) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    SecondParameter = parameter(InstructionPointer, 2, Intcode),
    ThirdParameter = parameter(InstructionPointer, 3, Intcode),
    StoragePosition = storage_pointer(ThirdParameter, ThirdMode, Intcode),

    NewIntcode =
        case parameter_value(FirstParameter, FirstMode, Intcode)
            < parameter_value(SecondParameter, SecondMode, Intcode)
        of
            'true' ->
                store(StoragePosition, 1, Intcode);
            'false' ->
                store(StoragePosition, 0, Intcode)
        end,
    step(4, NewIntcode).

%% Opcode 8 is equals: if the first parameter is equal to the second
%% parameter, it stores 1 in the position given by the third
%% parameter. Otherwise, it stores 0.
equal_to(#{'instruction_pointer' := InstructionPointer}=Intcode, [FirstMode, SecondMode, ThirdMode]) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    SecondParameter = parameter(InstructionPointer, 2, Intcode),
    ThirdParameter = parameter(InstructionPointer, 3, Intcode),
    StoragePosition = storage_pointer(ThirdParameter, ThirdMode, Intcode),

    NewIntcode =
        case parameter_value(FirstParameter, FirstMode, Intcode)
            == parameter_value(SecondParameter, SecondMode, Intcode)
        of
            'true' ->
                store(StoragePosition, 1, Intcode);
            'false' ->
                store(StoragePosition, 0, Intcode)
        end,
    step(4, NewIntcode).

%% Opcode 9 adjusts the relative base by the value of its only
%% parameter. The relative base increases (or decreases, if the value
%% is negative) by the value of the parameter.
relative_base(#{'instruction_pointer' := InstructionPointer
               ,'relative_base' := RelativeBase
               }=Intcode
             ,[FirstMode]
             ) ->
    FirstParameter = parameter(InstructionPointer, 1, Intcode),
    AdjustBy = parameter_value(FirstParameter, FirstMode, Intcode),
    %%  io:format("    setting relative base to ~p + ~p = ~p~n", [RelativeBase, AdjustBy, RelativeBase + AdjustBy]),
    step(2, Intcode#{'relative_base' => RelativeBase + AdjustBy}).


tests() ->
    %% Day 2 examples
    Tests = [{<<"1,9,10,3,2,3,11,0,99,30,40,50">>, 3500}
            ,{<<"1,0,0,0,99">>, 2}
            ,{<<"1,1,1,4,99,5,6,0,99">>, 30}
            ],
    run_tests(Tests).

run_tests(Tests) ->
    lists:foreach(fun run_test/1, Tests).

run_test({InputBin, ReturnCode}) ->
    case run(from_binary(InputBin)) of
        ReturnCode -> 'ok';
        _Return -> 'ok'
                   %%  io:format("~s failed to return ~p: ~p~n", [InputBin, ReturnCode, _Return])
    end.
