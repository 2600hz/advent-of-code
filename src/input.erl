-module(input).

-export([input/1
        ,sample/1
        ]).

input(Module) ->
    read_input(<<Module/binary, ".input">>).

sample(Module) ->
    read_input(<<Module/binary, ".sample">>).

read_input(File) ->
    {'ok', CWD} = file:get_cwd(),
    {'ok', Bin} = file:read_file(filename:join([CWD, "priv", File])),
    Bin.
