package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type Write struct {
	Address int64
	Value   int64
	Mask    string
}

func readInput() []Write {
	// our return value will be this slice of strings
	instrs := make([]Write, 0)

	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal("no input file provided!")
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	var currMask string
	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, "mask") {
			expr := strings.Split(line, "=")
			mask := expr[1]
			currMask = mask
		} else {
			w := Write{}
			re := regexp.MustCompile(`\d+`)
			ints := re.FindAllString(line, -1)
			addr, _ := strconv.Atoi(ints[0])
			val, _ := strconv.Atoi(ints[1])
			w.Address, w.Value, w.Mask = int64(addr), int64(val), currMask[1:]
			instrs = append(instrs, w)
		}
	}
	return instrs
}

func numToBinaryString(num int64) string {
	s := strconv.FormatInt(int64(num), 2)
	strArr := make([]string, 36)
	pad := len(strArr) - len(s)
	for i := 0; i < pad; i++ {
		strArr = append(strArr, "0")
	}
	for _, ru := range s {
		strArr = append(strArr, string(ru))
	}
	return strings.Join(strArr, "")
}

func binaryStringToNum(s string) int64 {
	num, err := strconv.ParseInt(s, 2, 64)
	if err != nil {
		panic("passed in a string that could not be converted to a number")
	}
	return num
}

func sol1(instrs []Write) int64 {
	memory := make(map[int64]int64)
	finalSum := int64(0)
	for _, instr := range instrs {
		ogNumStr := numToBinaryString(instr.Value)
		newNumArr := make([]string, 36)
		for i := len(instr.Mask) - 1; i >= 0; i-- {
			char := string(instr.Mask[i])
			if char == "1" {
				newNumArr[i] = "1"
			} else if char == "0" {
				newNumArr[i] = "0"
			} else {
				newNumArr[i] = string(ogNumStr[i])
			}
		}
		binStr := strings.Join(newNumArr, "")
		num := binaryStringToNum(binStr)
		memory[instr.Address] = num
	}

	for _, v := range memory {
		finalSum += v
	}
	return finalSum
}

func sol2Helper(count int, addr string, instr Write, mem map[int64]int64) {
	if count == 36 {
		addrNum := binaryStringToNum(addr)
		mem[addrNum] = instr.Value
		return
	}
	if string(instr.Mask[count]) == "1" {
		addr = addr[0:count] + "1" + addr[count+1:]
	}
	if string(instr.Mask[count]) == "X" {
		newZAddr := addr[0:count] + "0" + addr[count+1:]
		newOAddr := addr[0:count] + "1" + addr[count+1:]

		sol2Helper(count+1, newZAddr, instr, mem)
		sol2Helper(count+1, newOAddr, instr, mem)
	} else {
		sol2Helper(count+1, addr, instr, mem)
	}
}

func sol2(instrs []Write) int64 {
	memory := make(map[int64]int64)
	finalSum := int64(0)

	for _, instr := range instrs {
		binStr := numToBinaryString(instr.Address)
		sol2Helper(0, binStr, instr, memory)
	}

	for _, v := range memory {
		finalSum += v
	}
	return finalSum

}

func main() {
	in := readInput()
	solpt1 := sol1(in)
	solpt2 := sol2(in)
	fmt.Printf("The answer to part 1 is: %d\n", solpt1)
	fmt.Printf("The answer to part 2 is: %d\n", solpt2)
}
