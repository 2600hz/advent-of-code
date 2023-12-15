# Day 15: Lens Library

## Puzzle

<https://adventofcode.com/2023/day/15>

## Compile

```erlang
c(day15).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day15:hash_sum(Input). %% Part 1
day15:focusing_power(Input). %% Part 2
```
