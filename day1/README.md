# Advent of Code 2019 - Day 1 Solution

Solution for Advent of Code 2019 - Day 1 Problems 1 & 2

## Overview

1. Added text file **day1.input** with my input
2. Created functions to read and process **day1.input**. This includes splitting input into a list by `\n`, deleting the blank line at the end, and converting each element from binary to integer
3. Created function to calculate the fuel required to carry each individual module
4. Created function to calculate the fuel required to carry the fuel of each module
5. Created function to calculate the total fuel required by each module. This function uses the functions created in steps 3 and 4
6. Created functions to calculate the solutions of problem 1 and problem 2. These functions read and process the input, and use list comprehension to calculate the fuel required for each module. Finally it uses lists:sum to calculate the total fuel required.

## How to run

1. Compile day1.erl
2. Solution to problem 1 `day1:calc_fuel_for_modules().`
3. Solution to problem 2 `day1:calc_total_fuel().`
