def sol1(in_array):
    gamma_str = ""
    epsilon_str = ""
    for i in range(len(in_array[0]) - 1):
        ones = 0
        zeroes = 0
        for line in in_array:
            if line[i] == "1":
                ones += 1
            else:
                zeroes += 1
        if ones > zeroes:
            gamma_str += "1"
            epsilon_str += "0"
        else:
            gamma_str += "0"
            epsilon_str += "1"
    
    return int(gamma_str, 2) * int(epsilon_str, 2)


def sol2(in_array):
    oxy_list = in_array
    co2_list = in_array
    for i in range(len(oxy_list[0]) - 1):
        ones = 0
        zeroes = 0
        for line in oxy_list:
            if line[i] == "1":
                ones += 1
            else:
                zeroes += 1
        if ones >= zeroes:
            if len(oxy_list) > 1:
                oxy_list = list(filter(lambda x: x[i] == "1", oxy_list))
        else:
            if len(oxy_list) > 1:
                oxy_list = list(filter(lambda x: x[i] == "0", oxy_list))

    for i in range(len(co2_list[0]) - 1):
        ones = 0
        zeroes = 0
        for line in co2_list:
            if line[i] == "1":
                ones += 1
            else:
                zeroes += 1
        if ones >= zeroes:
            if len(co2_list) > 1:
                co2_list = list(filter(lambda x: x[i] == "0", co2_list))
        else:
            if len(co2_list) > 1:
                co2_list = list(filter(lambda x: x[i] == "1", co2_list))
    
    return int(oxy_list[0], 2) * int(co2_list[0], 2)


with open('input.txt', 'r') as r:
    in_array = r.readlines()

print(sol1(in_array))
print(sol2(in_array))