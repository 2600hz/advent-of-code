-module(day20).

-export([pulse_product/1
        ,button_presses_required/1
        ]).

-record(module, {type, dests, state, sent = {0, 0}}).
-record(pulse, {src, dest, val}).

pulse_product(Input) ->
    Config = parse_module_config(Input),
    Config1 = press_button(1000, Config),
    SentCounts = [Sent || #module{sent = Sent} <- maps:values(Config1)],
    {HighCounts, LowCounts} = lists:unzip(SentCounts),
    lists:sum(HighCounts) * lists:sum(LowCounts).

button_presses_required(Input) ->
    Config = parse_module_config(Input),
    ParentsOfRx = lists:uniq([Name || {Name, #module{dests = Dests}} <- maps:to_list(Config),
                                      lists:member(<<"rx">>, Dests)
                             ]),
    Options = [{'pulse_callback', pulse_recorder(ParentsOfRx)}],
    try press_button('infinity', Config, Options)
    catch
        {'pulses_recorded', Records} ->
            lists:foldl(fun math2:lcm/2, 1, [I || {_, I} <- Records])
    end.

pulse_recorder(Dests) ->
    fun(#pulse{dest = Dest, val = 'high'}=Pulse, I) ->
            lists:member(Dest, Dests) andalso record_pulse(Pulse, I);
       (_, _) -> 'ok'
    end.

record_pulse(Pulse, I) ->
    Records = case get('pulse_records') of
                  'undefined' -> [];
                  Records0 -> Records0
              end,

    case lists:keymember(Pulse, 1, Records) of
        'true' -> throw({'pulses_recorded', Records});
        'false' -> 'ok'
    end,

    io:format("high pulse ~p after ~b button presses~n", [Pulse, I + 1]),
    Records1 = [{Pulse, I + 1} | Records],
    put('pulse_records', Records1).

press_button(N, Config) -> press_button(N, Config, []).

press_button(N, Config, Options) -> press_button(0, N, Config, Options).

press_button(N, N, Config, _) -> Config;
press_button(I, N, Config, Options) ->
    Pulse = #pulse{dest = <<"button">>, val = 'low'},
    Config1 = process_pulses(queue:in(Pulse, queue:new()), Config, Options, I),
    press_button(I + 1, N, Config1, Options).

process_pulses(Queue, Config, Options, Context) ->
    case queue:out(Queue) of
        {'empty', _} -> Config;
        {{'value', Pulse}, Queue1} ->
            {Queue2, Config1} = process_pulse(Pulse, Queue1, Config, Options, Context),
            process_pulses(Queue2, Config1, Options, Context)
    end.

process_pulse(#pulse{src=Src, dest=Dest, val=V}=Pulse, Queue, Config, Options, Context)
  when is_map_key(Dest, Config) ->
    #module{type=Type
           ,dests=Dests
           ,state=State
           ,sent=Sent
           }=Module = maps:get(Dest, Config),
    case proplists:get_value('pulse_callback', Options) of
        'undefined' -> 'ok';
        Callback -> Callback(Pulse, Context)
    end,

    {ToSend, State1} = case Type of
                           'button' -> {'low', State};
                           'broadcaster' -> {V, State};
                           'flip_flop' -> process_flip_flop(V, State);
                           'conjunction' -> process_conjunction(Src, V, State)
                       end,

    case ToSend of
        'undefined' -> {Queue, Config};
        V1 ->
            PulseCount = length(Dests),
            Queue1 = lists:foldl(fun queue:in/2
                                ,Queue
                                ,[#pulse{src = Dest, dest = D, val = V1} || D <- Dests]
                                ),
            Module1 = Module#module{state = State1, sent = update_sent(V1, PulseCount, Sent)},
            Config1 = Config#{Dest => Module1},
            {Queue1, Config1}
    end;
process_pulse(_, Queue, Config, _, _) -> {Queue, Config}.

process_flip_flop('high', State) -> {'undefined', State};
process_flip_flop('low', 'off') -> {'high', 'on'};
process_flip_flop('low', 'on') -> {'low', 'off'}.

process_conjunction(Src, V, State) ->
    State1 = State#{Src => V},
    case lists:all(fun is_high/1, maps:values(State1)) of
        'true' -> {'low', State1};
        'false' -> {'high', State1}
    end.

is_high(V) -> V == 'high'.

update_sent('high', N, {H, L}) -> {H + N, L};
update_sent('low', N, {H, L}) -> {H, L + N}.

parse_module_config(Input) ->
    Modules = binary:split(Input, <<"\n">>, ['global']),
    Module = #module{type = 'button', dests = [<<"broadcaster">>]},
    Config = lists:foldl(fun add_module/2, #{<<"button">> => Module}, Modules),
    initialize_conjunctions(Config).

add_module(Module, Modules) ->
    {Type, Name, Dests, State} = parse_module(Module),
    Modules#{Name => #module{type = Type, dests = Dests, state = State}}.

parse_module(ModuleBin) ->
    [Module, Dests] = binary:split(ModuleBin, <<" -> ">>),
    {Type, Name, State} = parse_module_name(Module),
    {Type, Name, binary:split(Dests, <<", ">>, ['global']), State}.

parse_module_name(<<"broadcaster">>) -> {'broadcaster', <<"broadcaster">>, 'undefined'};
parse_module_name(<<$%, Name/binary>>) -> {'flip_flop', Name, 'off'};
parse_module_name(<<$&, Name/binary>>) -> {'conjunction', Name, #{}}.

initialize_conjunctions(Config) ->
    maps:fold(fun initialize_conjunctions/3, Config, Config).

initialize_conjunctions(_, #module{dests = []}, Config) -> Config;
initialize_conjunctions(ModuleName, #module{dests=[Dest | Dests]}, Config) ->
    Config1 = initialize_conjunction(Dest, ModuleName, Config),
    initialize_conjunctions(ModuleName, #module{dests=Dests}, Config1).

initialize_conjunction(Dest, ModuleName, Config) ->
    case maps:get(Dest, Config, 'undefined') of
        #module{type='conjunction', state=State}=Conjunction ->
            Config#{Dest => Conjunction#module{state = State#{ModuleName => 'low'}}};
        _ ->
            Config
    end.
