## Building

You need Erlang/OTP 21.3 in your path.

In `lib/aoc` run `make` to compile to helper library.

Then just run `./day3.escript` or `./day3-2.escript` to get the answers.

## Commentary

We assume the central port is at the origin {0, 0}.

1. Naively iterate the list of points a wire lies on into a `set()`.
2. Find the intersection of the two wires' set of points to find the `X` intersections.
3. Fold over the intersections, calculating the manhattan distance and keeping the smallest one.

### Part 2

The Steps are the same except instead of folding over the intersection points to find the smallest Manhattan distance, we need to fold over the intersection points, calculate the steps each wire takes to get there, and keep the smallest resulting step count.

I imagine a memoization solution could help decrease the work of calculating steps but for this problem we'll brute force it to see where we get.
