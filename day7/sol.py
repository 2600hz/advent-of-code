import re

class ElfDirectory:
    def __init__(self, pathname, parent=None):
        self.pathname = pathname
        self.parent = parent
        self.files = {}
        self.size = 0

    def get_name(self):
        return self.pathname

    def add_file(self, file):
        self.files[file.get_name()] = file

    def set_size(self, size):
        self.size = size
    
    def get_size(self):
        return self.size

    def get_file(self, filename):
        if filename not in self.files:
            raise Exception("no such element")
        return self.files[filename]

    def get_files(self):
        return list(self.files.values())

    def get_parent(self):
        return self.parent

class ElfFile:
    def __init__(self, parent, filename, size):
        self.filename = filename
        self.size = size
        self.parent = parent

    def get_name(self):
        return self.filename

    def get_size(self):
        return self.size

    def get_parent(self):
        return self.parent

def processCommands(commands):
    root_dir = ElfDirectory("/")
    curr_dir = root_dir
    for command in commands:
        if command[2:4] == "cd":
            new_dir = command[5:]
            if new_dir == "/":
                curr_dir = root_dir
            elif new_dir == "..":
                curr_dir = curr_dir.get_parent()
            else:
                curr_dir = curr_dir.get_file(new_dir)
        elif command[2:4] == "ls":
            pass
        else:
            if command[0:3] == "dir":
                curr_dir.add_file(ElfDirectory(command[4:], parent=curr_dir))
            else:
                file_groups = re.search("(\d+) (.+)", command).groups()
                size, name = int(file_groups[0]), file_groups[1]
                curr_dir.add_file(ElfFile(curr_dir, name, size))
    return root_dir

def part_one(directory):
    total = 0
    stack = [file for file in directory.get_files()]
    while len(stack) > 0:
        curr_node = stack.pop()
        curr_size = curr_node.get_size()
        curr_parent = curr_node.get_parent()
        parent_size = curr_parent.get_size()
        if type(curr_node) == ElfFile:
            curr_parent.set_size(parent_size + curr_size)
        else:
            if curr_size == 0:
                stack.append(curr_node)
                for file in curr_node.get_files():
                    stack.append(file)
            else:
                if curr_size < 100000:
                    total += curr_size
                curr_parent.set_size(parent_size + curr_size)
    return total

def part_two(directory):
    unused_space = 70000000 - directory.get_size()
    required = 30000000
    space_to_clear = required - unused_space
    closest = 100000000000000000
    stack = [directory]
    while len(stack) > 0:
        curr_node = stack.pop()
        if type(curr_node) == ElfDirectory:
            size = curr_node.get_size()
            diff = size - space_to_clear 
            if diff > 0 and diff < closest - space_to_clear:
                closest = size
            for file in curr_node.get_files():
                stack.append(file)

    return closest

with open("input.txt", "r") as r:
    raw_lines = [line.rstrip() for line in r.readlines()]
    filesystem = processCommands(raw_lines)

print("part one solutions is {}".format(part_one(filesystem)))
print("part two solutions is {}".format(part_two(filesystem)))