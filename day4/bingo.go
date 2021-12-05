package main

import (
	"fmt"
	"os"
	"bufio"
	"strconv"
	"strings"
)

type Xy struct {
	x int
	y int
}

func main() {
	file, _ := os.Open("input.txt")
	defer file.Close()
	sc := bufio.NewScanner(file)

	var idx int = 0
	var input [][]string
	for sc.Scan() {
		row := strings.Fields(sc.Text())
		if len(row) > 0 {
			input = append(input, row)
			idx++
		}
	}
	boards := make([][][]string, (idx-1)/5)
	var countIdx int = 0
	var boardsIdx int = 0
	for _, board := range(input[1:idx]) {
		if countIdx <= 4 {
			boards[boardsIdx] = append(boards[boardsIdx], board)
		}
		if countIdx == 4 {
			countIdx = 0
			boardsIdx++
		} else {
			countIdx++
		}
	}

	result := dayFour((idx-1)/5, input[0], boards)
	fmt.Println(result)
}

func dayFour(numberOfBoards int, input []string, boards[][][]string) int {

	strInput := strings.Split(input[0], ",")
	numbers := make([]int, len(strInput))
	boardsCounter := make([][]Xy, numberOfBoards)
	finalCount := 0
	 for idx, value := range(strInput) {
	 	number, _ := strconv.Atoi(value)
		numbers[idx] = number
		for i, board := range(boards) {

	 		currentBoardSize := len(boardsCounter[i])
			rtr := markBoards(number, board)

			if rtr.x != 100 {
				boardsCounter[i] = append(boardsCounter[i], rtr)
			}
	 		 newCurrentBoardSize := len(boardsCounter[i])
	 		 if currentBoardSize < newCurrentBoardSize && newCurrentBoardSize > 5 {
	 		 	maybeStop := verifyBoardNumbers(number, board, boardsCounter[i])
	 		 	if maybeStop == 1 {
	 		 		 for _,row:=range(board){
						   for _,col:=range(row){
							   if col!="100"{
								num, _ := strconv.Atoi(string(col))
								finalCount=finalCount+num
							   }
						   }	   
					   }
			     	return finalCount*number
	 		 	}
	 		 }
		}
	 }
	return 0
}

func markBoards(value int, board [][]string) Xy {
	for iRow, row := range(board) {
		for iCol, number :=  range(row) {
			num, _ := strconv.Atoi(string(number))
			if value == num {
				return Xy{iRow, iCol}
			}
		}
	}
	return Xy{100,100}
}

func verifyBoardNumbers(lastCall int, board[][]string, indexes []Xy) int {
	for _, dot := range(indexes) {
		x := dot.x
		y := dot.y
		board[x][y] = "100"
	}

	colCount := make([]int, 5)
	for i, row := range(board) {
		rowCount  := 0
		for _, col := range(row) {
			num, _ := strconv.Atoi(string(col))
			rowCount = rowCount+num
			colCount[i] = colCount[i]+num
		}
		if rowCount == 500 {
			return 1
		}
	}
	for i:=0;i<5;i++ {
		if colCount[i] == 500 {
			return 1
		}
	}
 	return 0
}
