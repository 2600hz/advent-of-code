# ugly day, I was trying to go fast :P

win = 6
loss = 0
draw = 3

rock = 1
paper = 2
scissors = 3

def part_one(rounds):
    total_score = 0
    for rps_round in rounds:
        if rps_round[0] == "A": # rock
            if rps_round[1] == "X":
                total_score += draw + rock
            elif rps_round[1] == "Y": #paper 
                total_score += win + paper
            elif rps_round[1] == "Z":
                total_score += loss + scissors
        elif rps_round[0] == "B": # paper
            if rps_round[1] == "X": # rock
                total_score += loss + rock
            elif rps_round[1] == "Y": # paper
                total_score += draw + paper
            elif rps_round[1] == "Z":
                total_score += win + scissors
        elif rps_round[0] == "C": #scissors
            if rps_round[1] == "X": #rock
                total_score += win + rock
            elif rps_round[1] == "Y": #paper
                total_score += loss + paper
            elif rps_round[1] == "Z":
                total_score += draw + scissors
    return total_score

def part_two(rounds):
    total_score = 0
    for rps_round in rounds:
        if rps_round[0] == "A": # rock
            if rps_round[1] == "X":
                total_score += loss + scissors
            elif rps_round[1] == "Y": #paper 
                total_score += draw + rock
            elif rps_round[1] == "Z":
                total_score += win + paper
        elif rps_round[0] == "B": # paper
            if rps_round[1] == "X": # rock
                total_score += loss + rock
            elif rps_round[1] == "Y": # paper
                total_score += draw + paper
            elif rps_round[1] == "Z":
                total_score += win + scissors
        elif rps_round[0] == "C": #scissors
            if rps_round[1] == "X": #rock
                total_score += loss + paper
            elif rps_round[1] == "Y": #paper
                total_score += draw + scissors
            elif rps_round[1] == "Z":
                total_score += win + rock
    return total_score

with open('input.txt', 'r') as r:
    pairs = [[char.rstrip() for char in line.split(" ")] for line in r.readlines()]

print("part 1 solution is {}".format(part_one(pairs)))
print("part 2 solution is {}".format(part_two(pairs)))
