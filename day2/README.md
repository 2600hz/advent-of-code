## Building

You need Erlang/OTP 21.3 in your path.

In `lib/aoc` run `make` to compile to helper library.

Then just run `./day1.escript` or `./day1-2.escript` to get the answers.

## Commentary

Since we don't actually need the list of masses themselves, just a running total, we could change this to calculate the fuel requirements on the fly (memory savings). If the input list was larger (only 100 lines here), this would be the approach to not exhaust the system's resources.
