#pt1
with open('input.txt', 'r') as r:p = r.readlines();a = [[line[i] for line in p] for i in range(len(p[0]) - 1)];print(int("".join(["1" if char_list.count("1") >= char_list.count("0") else "0" for char_list in a]), 2) * int("".join(["0" if char_list.count("1") >= char_list.count("0") else "1" for char_list in a]), 2))
#pt2
oxy_r,co2_r,curr=p,p,0
while len(oxy_r)>1:
    a = [[line[i] for line in oxy_r] for i in range(len(oxy_r[0]) - 1)];oxy_r = list(filter(lambda x: x[curr] == "1", oxy_r)) if a[curr].count("1") >= a[curr].count("0") else list(filter(lambda x: x[curr] == "0", oxy_r));curr+=1
curr=0
while len(co2_r)>1:
    a = [[line[i] for line in co2_r] for i in range(len(co2_r[0]) - 1)];co2_r = list(filter(lambda x: x[curr] == "0", co2_r)) if a[curr].count("1") >= a[curr].count("0") else list(filter(lambda x: x[curr] == "1", co2_r));curr+=1
print(int(oxy_r[0], 2) * int(co2_r[0], 2))