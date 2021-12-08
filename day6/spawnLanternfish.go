package main

import (
	"fmt"
	"os"
	"bufio"
	"strconv"
	"strings"
)

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()
	sc := bufio.NewScanner(file)

	internalTimers := []int{}
	for sc.Scan() {
		row := strings.Fields(sc.Text())
		for _, v := range(strings.Split(row[0], ",")) {
			num, _ := strconv.Atoi(v)
			internalTimers = append(internalTimers, num)
		}
	}
	result := daySix(internalTimers)
	fmt.Println("result: ", result)
}

func daySix(initialDays []int) int {
	cycle := inputEntry(initialDays)
	var inc int = 0
	for i:=0;i<80;i++ {
		cycle = spawnNew(inc, cycle)

		inc = 0
		for i, day := range(cycle) {
			if day == 0 {
				inc++
				cycle[i] = 6
			} else {
				cycle[i] = cycle[i]-1
			}
		}
	}
	return len(cycle)+inc
}

func inputEntry(input []int) map[int]int {
	cycle := make(map[int]int)
	for i, d :=range(input) {
		cycle[i]=d
	}
	return cycle
}

func spawnNew(count int, cycle map[int]int) map[int]int {
	for i:=0;i<count;i++ {
		cycle[len(cycle)+1] = 8
	}
	return cycle
}
