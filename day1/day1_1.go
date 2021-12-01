package main

import (
	"fmt"
	"os"
	"bufio"
	"strconv"
)

func main(){
	input, _ := os.Open("input.txt")
	defer input.Close()
	sc := bufio.NewScanner(input)
	
	depths := []int{}
	for sc.Scan() {
		depth, _ := strconv.Atoi(sc.Text())
		depths = append(depths, depth)
	}
	result := dayOne(depths)
	fmt.Println("Result: ", result)
}

func dayOne(depths []int) int {
	var count int = 0
	var tmp int = depths[0]
	for _, depth := range(depths) {
		if depth > tmp {
			count++
		}
		tmp = depth
	}
	return count
}