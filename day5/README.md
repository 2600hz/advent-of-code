# Day 5: If You Give A Seed A Fertilizer

## Puzzle

<https://adventofcode.com/2023/day/5>

## Compile

```erlang
c(day5).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day5:lowest_location(Input, 1). %% Part 1
day5:lowest_location(Input, 2). %% Part 2
```
