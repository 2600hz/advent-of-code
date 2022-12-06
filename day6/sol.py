def sol(line, offset):
    for i in range(offset-1, len(line)):
        if len(set(line[i-(offset-1):i+1])) == offset:
            return i+1

with open("input.txt", "r") as r:
    raw_line = r.readlines()[0]

print("part one solutions is {}".format(sol(raw_line, 4)))
print("part two solutions is {}".format(sol(raw_line, 14)))