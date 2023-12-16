# Day 16: The Floor Will Be Lava

## Puzzle

<https://adventofcode.com/2023/day/16>

## Compile

```erlang
c(day16).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day16:energized_tiles_count(Input, 1). %% Part 1
day16:energized_tiles_count(Input, 2). %% Part 2
```
