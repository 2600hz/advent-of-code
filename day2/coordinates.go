package main

import (
	"fmt"
	"os"
	"bufio"
	"strconv"
	"strings"
	// "reflect"
)

func main(){
	input, _ := os.Open("input.txt")
	defer input.Close()
	sc := bufio.NewScanner(input)

	var horizontalPosition int = 0
	var depthPosition int = 0
	for sc.Scan() {
		coordinate := strings.Fields(sc.Text())
		value, _ := strconv.Atoi(coordinate[1])
		switch coordinate[0] {
		case "forward":
			horizontalPosition = horizontalPosition + value
		case "down":
			depthPosition = depthPosition + value
		case "up":
			depthPosition = depthPosition - value
		default:
			continue
		}
	}
	fmt.Println("Result: ", horizontalPosition*depthPosition)
}