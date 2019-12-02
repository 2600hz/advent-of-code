## Building

You need Erlang/OTP 21.3 in your path.

In `lib/aoc` run `make` to compile to helper library.

Then just run `./day2.escript` or `./day2-2.escript` to get the answers.

## Commentary

Intcode programs require tracking the "position" in the program (InstructionPointer) as well as the data (opcode instruction or parameter). In most languages with destructive updating, we'd use a 0-indexed array. In Erlang, maps are the closest and easiest datastructure to represent the Intcode. `#{position() => data()}` gives us the ability to have random-access reading and writing to the Intcode program.

Using macros to represent the opcodes gives us more readable code than having to remember what magic numbers represent. NO MAGIC NUMBERS! :)
