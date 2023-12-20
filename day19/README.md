# Day 19: Aplenty

## Puzzle

<https://adventofcode.com/2023/day/19>

## Compile

1. Compile the [`aoc_utils`](../aoc_utils/) app.
2. Compile the solution module.

    ```erlang
    c(day19).
    ```

## Run

```shell
erl -pa ../aoc_utils/_build/default/lib/aoc_utils/ebin
```

```erlang
{'ok', Input} = file:read_file("input.txt").
day19:accepted_rating_number_total(Input). %% Part 1
day19:distinct_accepted_combinations_count(Input). %% Part 2
```
