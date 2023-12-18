# Day 5: If You Give A Seed A Fertilizer

## Puzzle

<https://adventofcode.com/2023/day/5>

## Compile

1. Compile the [`aoc_utils`](../aoc_utils/) app.
2. Compile the solution module.

    ```erlang
    c(day5).
    ```

## Run

```shell
erl -pa ../aoc_utils/_build/default/lib/aoc_utils/ebin
```

```erlang
{'ok', Input} = file:read_file("input.txt").
day5:lowest_location(Input, 1). %% Part 1
day5:lowest_location(Input, 2). %% Part 2
```
