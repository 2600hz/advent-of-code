import re
import copy

def sol1(nums, boards):
    for num in nums:
        for board in boards:
            replace_number(num, board)
            if is_winner(board):
                return determine_winner(num, board)

def replace_number(num, board):
    for i in range(len(board)):
        for j in range(len(board[i])):
            if num == board[i][j]:
                board[i][j] = "X"

def is_winner(board):
    for i in range(len(board)):
        if board[i] == ["X", "X", "X", "X", "X"]:
            return True
        cols = [board[j][i] for j in range(0, 5)]
        if cols == ["X", "X", "X", "X", "X"]:
            return True
    return False

def determine_winner(num, board):
    summed = 0
    for i in range(len(board)):
        for j in range(len(board[i])):
            if board[i][j] != "X":
                summed += int(board[i][j])
    return int(num) * summed

def sol2(nums, boards):
    won = 0
    all_but_last_found = False
    won_boards = set()
    last = 0
    boards_copy = copy.deepcopy(boards)
    for num in nums:
        if all_but_last_found:
            break
        for idx, board in enumerate(boards):
            if idx not in won_boards:
                replace_number(num, board)
                if is_winner(board):
                    won_boards.add(idx)
                    if len(won_boards) == len(boards) - 1:
                        all_but_last_found = True

    losing_idx = 0
    for idx in range(len(boards)):
        if idx not in won_boards:
            losing_idx = idx

    last_board = boards_copy[losing_idx]
    for num in nums:
        replace_number(num, last_board)
        if is_winner(last_board):
            return determine_winner(num, last_board)

with open('input.txt', 'r') as r:
    in_array = r.readlines()
    nums = [char.rstrip() for char in re.findall(r'[0-9]+', in_array[0])]
    boards = []
    curr_board = []
    for line in in_array[2:]:
        if line == "\n":
            boards.append(curr_board)
            curr_board = []
        else:
            curr_board.append([char.rstrip() for char in re.findall(r'[^ ]+', line)])
    boards.append(curr_board)

print(sol1(nums, boards))
print(sol2(nums, copy.deepcopy(boards)))