import re

def sol1(in_array):
    depth = 0
    horiz = 0
    for line in in_array:
        direction, units = line.split(" ")
        if direction == "forward":
            horiz += int(units)
        elif direction == "down":
            depth += int(units)
        else:
            depth -= int(units)
    
    return depth * horiz

def sol2(in_array):
    depth = 0
    horiz = 0
    aim = 0
    for line in in_array:
        direction, units = line.split(" ")
        if direction == "forward":
            horiz += int(units)
            depth += aim * int(units)
        elif direction == "down":
            aim += int(units)
        else:
            aim -= int(units)
    
    return depth * horiz

with open('input.txt', 'r') as r:
    in_array = r.readlines()

print(sol1(in_array))
print(sol2(in_array))