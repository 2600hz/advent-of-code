# Day 22: Sand Slabs

## Puzzle

<https://adventofcode.com/2023/day/22>

## Compile

```erlang
c(day22).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day22:disintegrated_bricks(Input). %% Part 1
day22:falling_bricks(Input). %% Part 2
```
