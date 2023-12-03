# Day 3: Gear Ratios

## Puzzle

<https://adventofcode.com/2023/day/3>

## Compile

```erlang
c(day3).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day3:sum_part_numbers(Input). %% Part 1
day3:sum_gear_ratios(Input). %% Part 2
```
