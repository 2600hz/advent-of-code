package main

import (
	"fmt"
	"os"
	"bufio"
	"strings"
	"strconv"
)

func main()  {
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
	binarySize := len(binaries[0])
	oxygendata := binaries
	scrubberData := binaries
	var scrubber string = ""
	for i:=0; i<binarySize; i++ {
		oxygen := oxygenGeneratorRating(i, oxygendata)
		if len(oxygendata) > 1 {
			oxygendata = filterByMostCommon(oxygendata, oxygen, i)
		}
		scrubber = scrubberRating(i, scrubberData)
		if len(scrubberData) > 1 {
			scrubberData = filterByMostCommon(scrubberData, scrubber, i)
		}	
	}
	oxy := convertData(oxygendata)
	scr := convertData(scrubberData)
	
	return oxy*scr
}

func convertData(binaries [][]string) int64 {
	var strBinary string = ""
	for i:=0; i<len(binaries[0]); i++ {
		strBinary = strBinary + binaries[0][i]
	}
	num, _ := strconv.ParseInt(strBinary, 2, 64)
	return num
}

func filterByMostCommon(binaries [][]string, mostCommonBinary string, position int) [][]string {
	var remainingList [][]string
	for i:=0; i<len(binaries); i++ {
		if binaries[i][position] == mostCommonBinary {
			remainingList = append(remainingList, binaries[i])
		}
	}
	return remainingList
}

func oxygenGeneratorRating(position int, binaries [][]string) string {
	binariesSize := len(binaries)
	var CountZero int = 0
	var CountOne int = 0
	for i:=0; i<binariesSize; i++ {
		switch binaries[i][position] {
		case "0":
			CountZero++
		case "1":
			CountOne++
		}
	}

	if CountZero <= CountOne {
		return "1"
	} else {
		return "0"
	}
}

func scrubberRating(position int, binaries [][]string) string {
	binariesSize := len(binaries)
	var CountZero int = 0
	var CountOne int = 0
	for i:=0; i<binariesSize; i++ {
		switch binaries[i][position] {
		case "0":
			CountZero++
		case "1":
			CountOne++
		}
	}

	if CountZero <= CountOne {
		return "0"
	} else {
		return "1"
	}
}