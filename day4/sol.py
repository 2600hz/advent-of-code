import re

def part_one(groups):
    total = 0
    for group in groups:
        s1 = set([i for i in range(group[0], group[1]+1)])
        s2 = set([i for i in range(group[2], group[3]+1)])
        summed = s1 & s2
        if summed == s1 or summed == s2:
            total += 1
    return total

def part_two(groups):
    total = 0
    for group in groups:
        s1 = set([i for i in range(group[0], group[1]+1)])
        s2 = set([i for i in range(group[2], group[3]+1)])
        if len(s1 &s2) != 0:
            total += 1
    return total

with open("input.txt", "r") as r:
    raw_lines = r.readlines()
    group_ranges = [[int(elem) for elem in re.findall("\d+", line)] for line in raw_lines]

print("part one solutions is {}".format(part_one(group_ranges)))
print("part two solutions is {}".format(part_two(group_ranges)))