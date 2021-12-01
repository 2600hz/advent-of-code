def part1(in_array):
    total = 0
    for i in range(1, len(in_array)):
        if in_array[i] > in_array[i - 1]:
            total += 1
    return total

def part2(in_array):
    total = 0
    last = 0
    for i in range(3, len(in_array)):
        window_sum = in_array[i] + in_array[i - 1] + in_array[i - 2]
        if window_sum > last:
            total += 1
        last = window_sum
    return total

with open('pt1_input.txt', 'r') as r:
    in_array = [int(line) for line in r.readlines()]

print(part1(in_array))
print(part2(in_array))