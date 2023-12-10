# Day 10: Pipe Maze

## Puzzle

<https://adventofcode.com/2023/day/10>

## Compile

```erlang
c(day10).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day10:steps_farthest_from_start(Input). %% Part 1
day10:enclosed_tiles_count(Input). %% Part 2
```
