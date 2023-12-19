# Day 6: Wait For It

## Puzzle

<https://adventofcode.com/2023/day/6>

## Compile

1. Compile the [`aoc_utils`](../aoc_utils/) app.
2. Compile the solution module.

    ```erlang
    c(day6).
    ```

## Run

```shell
erl -pa ../aoc_utils/_build/default/lib/aoc_utils/ebin
```

```erlang
{'ok', Input} = file:read_file("input.txt").
day6:record_ways_product(Input, 1). %% Part 1
day6:record_ways_product(Input, 2). %% Part 2
```
