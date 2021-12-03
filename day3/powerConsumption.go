package main

import (
	"fmt"
	"os"
	"bufio"
	"strings"
	"strconv"
)

func main() {
	input, _ := os.Open("input.txt")
	defer input.Close()
	sc := bufio.NewScanner(input)
	
	strBinaries := [][]string{}
	for sc.Scan() {
		binary := strings.Fields(sc.Text())
		strBinaries = append(strBinaries, strings.SplitAfter(binary[0], ""))
	}
	result := dayThree(strBinaries)
	fmt.Println(result)
}

func dayThree(binaries [][]string) int64 {
	binariesSize := len(binaries)
	binarySize := len(binaries[0])
	var mostCommon string = ""
	var inverseCommon string = ""
	for i:=0; i<binarySize; i++ {
		var CountZero int = 0
		var CountOne int = 0
		for n:=0; n<binariesSize; n++ {
			switch binaries[n][i] {
			case "0":
				CountZero++
			case "1":
				CountOne++
			}
		}
		if CountZero < CountOne {
			mostCommon = mostCommon + "1"
			inverseCommon = inverseCommon + "0"
		} else {
			mostCommon = mostCommon + "0"
			inverseCommon = inverseCommon + "1"
		}
	}
	gamma, _ := strconv.ParseInt(mostCommon, 2, 64)
	epsilon, _ := strconv.ParseInt(inverseCommon, 2, 64)
	return gamma*epsilon
}