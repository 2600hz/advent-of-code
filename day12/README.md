# Day 12: Hot Springs

## Puzzle

<https://adventofcode.com/2023/day/12>

## Compile

```erlang
c(day12).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day12:sum_arrangements(Input, 1). %% Part 1
day12:sum_arrangements(Input, 2). %% Part 2
```
