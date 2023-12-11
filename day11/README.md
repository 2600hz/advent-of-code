# Day 11: Cosmic Expansion

## Puzzle

<https://adventofcode.com/2023/day/11>

## Compile

```erlang
c(day11).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day11:sum_distances_between_galaxies(Input, 1). %% Part 1
day11:sum_distances_between_galaxies(Input, 2). %% Part 2
```
