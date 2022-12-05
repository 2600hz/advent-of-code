import re
import copy

def prepareAnswer(stacks):
    s = ""
    for stack in stacks:
        s += stack.pop()
    return s

def part_one(crate_stacks, moves):
    stacks = copy.deepcopy(crate_stacks)
    for move in moves:
        for i in range(0, move[0]):
            elem = stacks[move[1]-1].pop()
            stacks[move[2]-1].append(elem)
    return prepareAnswer(stacks)

def part_two(crate_stacks, moves):
    stacks = copy.deepcopy(crate_stacks)
    for move in moves:
        crates_to_move = []
        for i in range(move[0]):
            elem = stacks[move[1]-1].pop()
            crates_to_move.insert(0, elem)
        stacks[move[2]-1] += crates_to_move
    return prepareAnswer(stacks)

with open("input.txt", "r") as r:
    raw_lines = r.readlines()
    raw_lines_copy = raw_lines[:]
    crates = list(filter(lambda x: "[" in x, raw_lines))
    moves = [move.rstrip() for move in list(filter(lambda x: "move" in x, raw_lines_copy))]

    built_crate_stacks = []
    for col in range(0, (len(crates[0])+1) // 4):
        built_crate_stacks.append([])

    for crate in crates:
        for idx, num in enumerate(range(1, len(crate), 4)):
            if crate[num] != ' ':
                stack_idx = idx
                built_crate_stacks[stack_idx].insert(0, crate[num])

    stripped_moves = [[int(num) for num in list(re.search("move (\d+) from (\d+) to (\d+)", move).groups())] for move in moves]

print("part one solutions is {}".format(part_one(built_crate_stacks, stripped_moves)))
print("part two solutions is {}".format(part_two(built_crate_stacks, stripped_moves)))