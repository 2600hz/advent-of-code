# Day 2: Cube Conundrum

## Puzzle

<https://adventofcode.com/2023/day/2>

## Compile

```erlang
c(day2).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day2:sum_possible_games(Input). %% Part 1
day2:sum_power_of_minimum_cube_sets(Input). %% Part 2
```
