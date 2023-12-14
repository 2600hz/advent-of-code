# Day 14: Parabolic Reflector Dish

## Puzzle

<https://adventofcode.com/2023/day/14>

## Python

### Run (Python)

```python
import day14

f = open('input.txt')
input = f.read()
day14.get_total_load(input, 1) ## Part 1
day14.get_total_load(input, 2) ## Part 2
```

## Erlang

### Compile

```erlang
c(day14).
```

### Run (Erlang)

```erlang
{'ok', Input} = file:read_file("input.txt").
day14:total_load(Input, 1). %% Part 1
day14:total_load(Input, 2). %% Part 2
```
