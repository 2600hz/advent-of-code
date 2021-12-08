def sol(in_array, days):
    curr_day = 0
    fish_max = max(in_array)
    fish_map = {k: 0 for k in range(-1, 9)}
    for item in in_array:
        fish_map[item] += 1

    while curr_day < days:
        temp = {k: 0 for k in range(-1, 9)}
        for key in fish_map:
            if key >= 0:
                temp[key-1] = fish_map[key]

        temp[8] = temp[-1]
        temp[6] += temp[-1]
        temp[-1] = 0

        fish_map = temp
        curr_day += 1

    summed = 0
    for key in fish_map:
        summed += fish_map[key]
    return summed

with open('input.txt', 'r') as r:
    lines = r.readlines()
    in_array = [int(char) for char in lines[0].split(',')]

print(sol(in_array, 80))
print(sol(in_array, 256))