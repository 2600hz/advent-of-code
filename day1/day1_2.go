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
	var totalCount int = 0
	var tmp int = depths[0] + depths[1] + depths[2]
	for i, _ := range(depths){
		if i <= len(depths) - 3 {
			currentSum := depths[i] + depths[i+1] + depths[i+2]
			if tmp < currentSum {
				totalCount++
			}
			tmp = currentSum
		}		
	}
	return totalCount
}