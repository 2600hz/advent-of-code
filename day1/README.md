# Day 1: Trebuchet?!

## Puzzle

<https://adventofcode.com/2023/day/1>

## Compile

```erlang
c(day1).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day1:sum_calibration_values(Input, 1). %% Part 1
day1:sum_calibration_values(Input, 2). %% Part 2
```
