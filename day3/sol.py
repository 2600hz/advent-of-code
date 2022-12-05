import string

weights = {}

def part_one(sack_pairs):
    total = 0
    for sack_pair in sack_pairs: # lol
        seen_set = set([])
        common_chars = set([])
        for char in sack_pair[0]:
            seen_set.add(char)

        for char in sack_pair[1]:
            if char in seen_set:
                common_chars.add(char)

        for char in common_chars:
            total += weights[char]
    return total

def part_two(sack_trios):
    total = 0
    for sack_trio in sack_trios:
        common_chars = ((set(list(sack_trio[0])) & set(list(sack_trio[1]))) & set(list(sack_trio[2])))
        for char in common_chars:
            total += weights[char]
    return total

for idx, char in enumerate(string.ascii_lowercase):
    weights[char] = idx + 1

for idx, char in enumerate(string.ascii_uppercase):
    weights[char] = idx + 1 + len(string.ascii_uppercase)

with open("input.txt", "r") as r:
    raw_lines = r.readlines()
    split_lines = [[line[0:(len(line)//2)], line[(len(line)//2):len(line)-1]] for line in raw_lines]
    line_trios = [[raw_lines[i].rstrip(), raw_lines[i-1].rstrip(), raw_lines[i-2].rstrip()] for i in range(2, len(raw_lines), 3)]

print("part one solutions is {}".format(part_one(split_lines)))
print("part two solutions is {}".format(part_two(line_trios)))