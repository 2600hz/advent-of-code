package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
)

const TARGET = 2020

func readInput() []int {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal("no input file provided!")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	in := make([]int, 0)
	for scanner.Scan() {
		i, _ := strconv.Atoi(scanner.Text())
		in = append(in, i)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	return in
}

func solutionPt1(target int, input []int) (int, bool) {
	solved := false
	// First, we sort
	sort.Ints(input)
	// Then, sliding window until we find the two integers
	start, end := 0, len(input)-1
	for start < end {
		if input[start]+input[end] > target {
			end -= 1
			continue
		} else if input[start]+input[end] == target {
			solved = true
			break
		} else {
			start += 1
			continue
		}
	}

	return input[start] * input[end], solved
}

func solutionPt2(target int, input []int) int {
	for i := 0; i < len(input); i++ {
		x := input[i]
		p, solved := solutionPt1(target-x, input[i:])
		if solved {
			return x * p
		} else {
			continue
		}
	}
	return 0
}

func main() {
	in := readInput()
	sol1, _ := solutionPt1(TARGET, in)
	fmt.Printf("Solution for part 1 is: %d!\n", sol1)
	sol2 := solutionPt2(TARGET, in)
	fmt.Printf("Solution for part 2 is: %d!\n", sol2)
}
