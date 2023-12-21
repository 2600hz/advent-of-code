# Day 20: Pulse Propagation

## Puzzle

<https://adventofcode.com/2023/day/20>

## Compile

1. Compile the [`aoc_utils`](../aoc_utils/) app.
2. Compile the solution module.

    ```erlang
    c(day20).
    ```

## Run

```shell
erl -pa ../aoc_utils/_build/default/lib/aoc_utils/ebin
```

```erlang
{'ok', Input} = file:read_file("input.txt").
day20:pulse_product(Input). %% Part 1
day20:button_presses_required(Input). %% Part 2
```
