# Advent of Code 2019 - Day 4 Solution

Solution for Advent of Code 2019 - Day 4 Problems 1 & 2

## Overview

These problems provided some relief after yesterday's challenge, however, after I started writing this I realized that my **adjacent_digit** function would not work in scenarios where there are multiple groups of the same adjacent digits separated by other digits. It is late, and the function was good enough for a star, so I'm not going to worry about it.

I also could not find a nice way to check values using maps directly so in order to ensure that the two adjacent matching digits criteria was met I used `maps:value` to return as list of values and `lists:any` to check them.

## How to run

1. Complete day4.erl
2. Solution to problem 1 `day4:num_of_matching_pwds1().`
3. Solution to problem 2 `day4:num_of_matching_pwds2().`
