## Building

You need Erlang/OTP 21.3 in your path.

In `lib/aoc` run `make` to compile to helper library.

Then just run `./day3.escript` or `./day3-2.escript` to get the answers.

## Commentary

We assume the central port is at the origin {0, 0}.

1. Naively iterate the list of points a wire lies on into a `set()`.
2. Find the intersection of the two wires' set of points to find the `X` intersections.
3. Fold over the intersections, calculating the manhattan distance and keeping the smallest one.
