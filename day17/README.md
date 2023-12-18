# Day 17: Clumsy Crucible

## Puzzle

<https://adventofcode.com/2023/day/17>

## Compile

```erlang
c(day17).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day17:minimum_heat_loss(Input, 1). %% Part 1
day17:minimum_heat_loss(Input, 2). %% Part 2
```
