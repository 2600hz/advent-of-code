package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func readInput() []int {
	// our return value will be this slice of strings
	ints := make([]int, 0)

	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal("no input file provided!")
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		strs := strings.Split(line, ",")
		for _, str := range strs {
			num, _ := strconv.Atoi(str)
			ints = append(ints, num)
		}
	}
	return ints
}

func contains(nums []int, i int) bool {
	for _, num := range nums {
		if i == num {
			return true
		}
	}
	return false
}

// sol1 solution was brute force, which came back to haunt me hehe
func sol1(nums []int) int {
	for i := len(nums) - 1; i < 2019; i++ {
		num := nums[i]
		hist := make([]int, 0)
		for j := i; j >= 0 && len(hist) < 3; j-- {
			if nums[j] == num {
				hist = append(hist, j)
			}
		}
		if len(hist) == 1 {
			nums = append(nums, 0)
		} else if len(hist) == 2 {
			nums = append(nums, hist[0]-hist[1])
		} else {
			nums = append(nums, hist[0]-hist[1])
		}
	}
	return nums[len(nums)-1]
}

// sol2 uses the update memory function below to not only write
// to a map that stores what numbers have been seen and what index,
// but to also add onto our original starting numbers
func sol2(nums []int) int {
	mem := make(map[int][]int)
	for i, num := range nums {
		arr, ok := mem[num]
		if !ok {
			mem[num] = make([]int, 0)
			mem[num] = append(mem[num], i)
		} else {
			arr = append(arr, i)
		}
	}
	for i := len(nums) - 1; i < 30000000-1; i++ { // 0th index shenanigans
		updateMemory(&nums, i, &mem)
	}
	return nums[len(nums)-1]
}

// the way i chose to keep memory as an object from a different function
// creates a lot of really ugly dereferencing, so bear with that
func updateMemory(nums *[]int, currIndex int, mem *map[int][]int) {
	num := (*nums)[currIndex]
	arr, ok := (*mem)[num]
	if !ok {
		(*mem)[num] = make([]int, 0)
		(*mem)[num] = append((*mem)[num], currIndex)
		*nums = append(*nums, 0)
	} else {
		if len(arr) == 1 {
			*nums = append(*nums, currIndex-arr[0])
		} else {
			*nums = append(*nums, arr[len(arr)-1]-arr[len(arr)-2])
		}
		num = (*nums)[len((*nums))-1]
		currIndex += 1
		(*mem)[num] = append((*mem)[num], currIndex)
	}
}

func main() {
	in := readInput()
	solpt1 := sol1(in)
	fmt.Printf("The answer to part 1 is: %d\n", solpt1)
	solpt2 := sol2(in)
	fmt.Printf("The answer to part 2 is: %d\n", solpt2)
}
