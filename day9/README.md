# Day 9: Mirage Maintenance

## Puzzle

<https://adventofcode.com/2023/day/9>

## Compile

```erlang
c(day9).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day9:sum_extrapolated_values(Input, 1). %% Part 1
day9:sum_extrapolated_values(Input, 2). %% Part 2
```
