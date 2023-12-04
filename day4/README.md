# Day 4: Scratchcards

## Puzzle

<https://adventofcode.com/2023/day/4>

## Compile

```erlang
c(day4).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day4:total_points(Input). %% Part 1
day4:total_scratchcards(Input). %% Part 2
```
