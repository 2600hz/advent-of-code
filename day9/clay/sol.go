package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
)

const (
	PREAMBLE = 25
	HISTORY  = 25
)

func readInput() []int {
	// our return value will be this slice of strings
	p := make([]int, 0)

	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal("no input file provided!")
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		num, _ := strconv.Atoi(line)
		p = append(p, num)
	}
	return p
}

func sol1(nums []int) (int, int) {
	cache := make([]int, 0)
	for i := 0; i < HISTORY; i++ {
		cache = append(cache, nums[i])
	}
	for i := PREAMBLE; i < len(nums); i++ {
		valid := false
		for j := 0; j < len(cache)-1; j++ {
			for k := j + 1; k < len(cache); k++ {
				if cache[j]+cache[k] == nums[i] {
					valid = true
				}
			}
		}
		if !valid {
			return i, nums[i]
		}
		cache = cache[1:]
		cache = append(cache, nums[i])
	}
	return 0, 0
}

func sol2(nums []int) (int, int) {
	index, value := sol1(nums)
	for i := 0; i < index; i++ {
		solution := make([]int, 0)
		sum := 0
		for j := i; j < index; j++ {
			if nums[j]+sum < value {
				sum += nums[j]
				fmt.Printf("add %d, sum is %d\n", nums[j], sum)
				solution = append(solution, nums[j])
				continue
			}
			if nums[j]+sum == value {
				solution = append(solution, nums[j])
				sum += nums[j]
				fmt.Printf("add %d, sum is %d\n", nums[j], sum)
				fmt.Printf("found a combination of numbers that equaled the target: %v\n", solution)
				sort.Ints(solution)
				return solution[0], solution[len(solution)-1]
			}
			if nums[j]+sum > value {
				sum += nums[j]
				fmt.Printf("add %d, sum is %d\n", nums[j], sum)
				solution = make([]int, 0)
				sum = 0
				fmt.Println("sum went over, doing next\n\n")
				break
			}
		}
	}
	return 0, 0
}

func main() {
	in := readInput()
	_, solpt1 := sol1(in)
	index2, solpt2 := sol2(in)
	fmt.Printf("The answer to part 1 is: %d\n", solpt1)
	fmt.Printf("The answer to part 2 is: %d\n", (index2 + solpt2))
}
