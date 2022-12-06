def part_one(line):
    for i in range(3, len(line)):
        if len(set(line[i-3:i+1])) == 4:
            return i+1

def part_two(line):
    for i in range(13, len(line)):
        if len(set(line[i-13:i+1])) == 14:
            return i+1

with open("input.txt", "r") as r:
    raw_line = r.readlines()[0]

print("part one solutions is {}".format(part_one(raw_line)))
print("part two solutions is {}".format(part_two(raw_line)))