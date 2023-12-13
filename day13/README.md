# Day 13: Point of Incidence

## Puzzle

<https://adventofcode.com/2023/day/13>

## Compile

```erlang
c(day13).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day13:summarize_notes(Input, 1). %% Part 1
day13:summarize_notes(Input, 2). %% Part 2
```
