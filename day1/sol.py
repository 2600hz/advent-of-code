def part_one(elves):
    max_cals = 0
    for elf in elves:
        max_cals = max(sum(elf), max_cals)
    return max_cals

def part_two(elves):
    all_sums = []
    for elf in elves:
        all_sums.append(sum(elf))
    sorted_sums = sorted(all_sums)
    print(sorted_sums[-1] + sorted_sums[-2] + sorted_sums[-3])

with open('input.txt', 'r') as r:
  raw_array = [line for line in r.readlines()]
  groups = []
  curr_group = []
  for line in raw_array:
    if line == "\n":
        groups.append(curr_group)
        curr_group = []
    else:
        curr_group.append(int(line))

print("part 1 solution is {}".format(part_one(groups)))
part_two(groups)