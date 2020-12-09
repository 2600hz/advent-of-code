package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type Maze struct {
	Lines [][]string
}

func readInput() Maze {
	maze := Maze{}
	maze.Lines = make([][]string, 0)
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal("no input file provided!")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]string, 0)
		for _, char := range line {
			row = append(row, string(char))
		}
		maze.Lines = append(maze.Lines, row)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	return maze
}

func solution(m Maze, downIncr int, rightIncr int) int {
	down := 0
	right := 0
	trees := 0
	for len(m.Lines)-1 > down {
		down += downIncr
		right += rightIncr
		if m.Lines[down][right%len(m.Lines[down])] == "#" {
			trees += 1
		}
	}
	return trees
}

func sol1(m Maze) int {
	return solution(m, 1, 3)
}

func sol2(m Maze) int {
	routes := [][]int{
		[]int{1, 1},
		[]int{1, 3},
		[]int{1, 5},
		[]int{1, 7},
		[]int{2, 1},
	}
	prod := 1
	for _, pairs := range routes {
		prod = prod * solution(m, pairs[0], pairs[1])
	}
	return prod
}

func main() {
	in := readInput()
	solpt1 := sol1(in)
	fmt.Printf("The answer to part 1 is: %d\n", solpt1)
	solpt2 := sol2(in)
	fmt.Printf("The answer to part 2 is: %d\n", solpt2)
}
