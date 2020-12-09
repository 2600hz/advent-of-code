package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
)

func readInput() []string {
	// our return value will be this slice of strings
	p := make([]string, 0)

	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal("no input file provided!")
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		p = append(p, line)
	}
	return p
}

func max(nums []int) int {
	top := -10000000000000
	for _, num := range nums {
		if num > top {
			top = num
		}
	}
	return top
}

func getSeatId(pos string) int {
	rowChars := pos[0:7]
	colChars := pos[7:10]
	// A sort of binary search encoding where we update front and back
	// depending on where the character wants us to check (top half vs bottom half)
	var rowNum int
	front := 0
	back := 127
	for _, ru := range rowChars { // rune
		char := string(ru) // string for easier comparison
		if char == "F" {
			back = (front + back) / 2
		} else {
			front = ((front + back) / 2) + 1
		}
	}
	rowNum = front // front and back should be the same value at this point
	// Now let's do the same for the last 3 characters (denoting the columns)
	var colNum int
	left := 0
	right := 7
	for _, ru := range colChars {
		char := string(ru)
		if char == "L" {
			right = (left + right) / 2
		} else {
			left = ((left + right) / 2) + 1
		}
	}
	colNum = left // same with left and right
	id := (rowNum * 8) + colNum
	return id
}

func getSeatIds(positions []string) []int {
	ids := make([]int, 0)
	for _, pos := range positions {
		id := getSeatId(pos)
		ids = append(ids, id)
	}
	return ids
}

func sol1(positions []string) int {
	ids := getSeatIds(positions)
	return max(ids)
}

func sol2(positions []string) int {
	missing := 0 // our return value, our boarding pass id
	// We're searching for a missing consecutive number!
	ids := getSeatIds(positions)
	sort.Ints(ids)
	for i := 1; i < len(positions)-1; i++ {
		if ids[i]+1 != ids[i+1] {
			missing = ids[i] + 1 // +1 because it's the id after this index that's missing
		}
	}
	return missing
}

func main() {
	in := readInput()
	solpt1 := sol1(in)
	fmt.Printf("The answer to part 1 is: %d\n", solpt1)
	solpt2 := sol2(in)
	fmt.Printf("The answer to part 2 is: %d\n", solpt2)
}
