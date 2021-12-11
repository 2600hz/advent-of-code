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
	result := daySeven(internalTimers)
	fmt.Println("result: ", result)
}

func daySeven(horizontal []int) int {
	positions := []int{}
	for i:=0; i<len(horizontal)+1; i++ {
		count := 0
		for _, position := range(horizontal) {
			if position > i {
				count = count + (position - i)
			} else if position < i {
				count = count + (i - position)
			}
		}
		positions = append(positions, count)
	}
	fmt.Println(positions)
	return get_smallest(positions)
}

func get_smallest(positions []int) int {
	s := 10000000
	for _, position :=range(positions) {
		if position < s {
			s = position
		}
	}
	return s
}
