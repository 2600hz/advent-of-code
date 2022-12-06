-module(input).

-export([input/1
        ,sample/1, samples/1
        ]).

input(Module) ->
    read_input(<<Module/binary, ".input">>).

sample(Module) ->
    read_input(<<Module/binary, ".sample">>).

samples(Module) ->
    read_inputs(<<Module/binary, ".?.sample">>).

read_input(File) ->
    {'ok', CWD} = file:get_cwd(),
    read_file(filename:join([CWD, "priv", File])).

read_inputs(File) ->
    {'ok', CWD} = file:get_cwd(),
    Files = filelib:wildcard(binary_to_list(filename:join([CWD, "priv", File]))),
    [read_file(F) || F <- Files].

read_file(File) ->
    {'ok', Bin} = file:read_file(File),
    Bin.
