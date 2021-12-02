package main

import (
	"fmt"
	"os"
	"bufio"
	"strconv"
	"strings"
)

func main(){
	input, _ := os.Open("input.txt")
	defer input.Close()
	sc := bufio.NewScanner(input)

	var horizontalPosition int = 0
	var depthPosition int = 0
	var aim int = 0
	for sc.Scan() {
		coordinate := strings.Fields(sc.Text())
		value, _ := strconv.Atoi(coordinate[1])
		switch coordinate[0] {
		case "forward":
			horizontalPosition = horizontalPosition + value
			depthPosition = depthPosition+(aim*value)
		case "down":
			aim = aim + value
		case "up":
			aim = aim - value
		default:
			continue
		}
	}
	fmt.Println("Result: ", horizontalPosition*depthPosition)
}