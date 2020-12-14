package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
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

func sol1(nums []int) int {
	sort.Ints(nums)
	power, oneDiffs, threeDiffs := 0, 0, 0
	i := 0
	for i < len(nums) {
		diff := nums[i] - power
		if diff == 3 {
			threeDiffs += 1
			power = nums[i]
		} else if diff == 2 {
			power = nums[i]
		} else if diff == 1 {
			oneDiffs += 1
			power = nums[i]
		} else {
			break
		}
		i += 1
	}
	threeDiffs += 1
	return oneDiffs * threeDiffs
}

func contains(ints []int, i int) bool {
	for _, num := range ints {
		if i == num {
			return true
		}
	}
	return false
}

func sol2(nums []int) int {
	nums = append(nums, 0)
	sort.Ints(nums)
	nums = append(nums, nums[len(nums)-1]+3)
	cache := make(map[int]int)
	cache[0] = 1
	for _, num := range nums {
		for diff := 1; diff < 4; diff++ {
			next := num + diff
			ok := contains(nums, next)
			if ok {
				cache[next] += cache[num]
			}
		}
	}
	return cache[nums[len(nums)-1]]
}

func main() {
	in := readInput()
	solpt1 := sol1(in)
	solpt2 := sol2(in)
	fmt.Printf("The answer to part 1 is: %d\n", solpt1)
	fmt.Printf("The answer to part 2 is: %d\n", solpt2)
}
