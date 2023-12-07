# Day 7: Camel Cards

## Puzzle

<https://adventofcode.com/2023/day/7>

## Compile

```erlang
c(day7).
```

## Run

```erlang
{'ok', Input} = file:read_file("input.txt").
day7:total_winnings(Input, 1). %% Part 1
day7:total_winnings(Input, 2). %% Part 2
```
