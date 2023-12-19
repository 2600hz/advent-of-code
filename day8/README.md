# Day 8: Haunted Wasteland

## Puzzle

<https://adventofcode.com/2023/day/8>

## Compile

1. Compile the [`aoc_utils`](../aoc_utils/) app.
2. Compile the solution module.

    ```erlang
    c(day8).
    ```

## Run

```shell
erl -pa ../aoc_utils/_build/default/lib/aoc_utils/ebin
```

```erlang
{'ok', Input} = file:read_file("input.txt").
day8:count_steps(). %% Part 1
day8:count_simultaneous_steps(Input). %% Part 2
```
