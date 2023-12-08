# Day 8: Haunted Wasteland

## Puzzle

<https://adventofcode.com/2023/day/8>

## Compile

```erlang
c(day8).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day8:count_steps(). %% Part 1
day8:count_simultaneous_steps(Input). %% Part 2
```
