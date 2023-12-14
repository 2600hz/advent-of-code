from enum import Enum
import numpy

Directions = Enum('Directions', ['left', 'up', 'right', 'down'])

_one_billion = 1_000_000_000

def get_total_load(input, part):
    grid = _parse_input(input)

    if part == 1:
        grid = _tilt(grid, Directions.up)
        return _get_load(grid)

    memory = {}
    for i in range(0, _one_billion):
        grid = _cycle(grid)
        grid_hash = hash(tuple(grid.flat))

        if grid_hash in memory:
            start, _ = memory[grid_hash]
            period = i - start
            row_loads = [v for (_, v) in memory.values()]
            return row_loads[start + (_one_billion - start - 1) % period]
        else:
            memory[grid_hash] = i, _get_load(grid)

def _get_load(grid):
    row_count = grid.shape[0]
    return sum([row.tolist().count('O') * (row_count - i)
                for [i, row] in enumerate(grid)
               ])

def _cycle(grid):
    grid = _tilt(grid, Directions.up)
    grid = _tilt(grid, Directions.left)
    grid = _tilt(grid, Directions.down)
    grid = _tilt(grid, Directions.right)
    return grid

def _tilt(grid, direction):
    grid = numpy.rot90(grid, direction.value - 1)

    new_grid = numpy.full(grid.shape, '.')
    for i in range(0, grid.shape[0]):
        nj = 0
        for j in range(0, grid.shape[1]):
            match grid[i][j]:
                case 'O':
                    new_grid[i][nj] = grid[i][j]
                    nj += 1
                case '#':
                    nj = j
                    new_grid[i][nj] = grid[i][j]
                    nj += 1

    return numpy.rot90(new_grid, -(direction.value - 1))

def _parse_input(input):
    return numpy.array([[x for x in line] for line in input.split('\n')])
