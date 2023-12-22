# Day 21: Step Counter

## Puzzle

<https://adventofcode.com/2023/day/21>

## Compile

1. Compile the [`aoc_utils`](../aoc_utils/) app.
2. Compile the solution module.

    ```erlang
    c(day21).
    ```

## Run

```shell
erl -pa ../aoc_utils/_build/default/lib/aoc_utils/ebin
```

```erlang
{'ok', Input} = file:read_file("input.txt").
day21:reachable_plots(Input, 1). %% Part 1
day21:reachable_plots(Input, 2). %% Part 2
```
