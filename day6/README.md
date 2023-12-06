# Day 6: Wait For It

## Puzzle

<https://adventofcode.com/2023/day/6>

## Compile

```erlang
c(day6).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day6:record_ways_product(Input, 1). %% Part 1
day6:record_ways_product(Input, 2). %% Part 2
```
