package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

func readInput() [][]string {
	// our return value will be this slice of strings
	ret := make([][]string, 0)

	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal("no input file provided!")
	}
	defer file.Close()

	inBytes, err := ioutil.ReadAll(file)
	if err != nil {
		log.Fatal(err)
	}
	inString := string(inBytes)
	groups := strings.Split(inString, "\n\n")
	for _, group := range groups {
		answers := make([]string, 0)
		ppl := strings.Split(group, "\n")
		for _, p := range ppl {
			if len(p) > 0 {
				answers = append(answers, p)
			}
		}
		ret = append(ret, answers)
	}
	return ret
}

func sol1(groups [][]string) int {
	sum := 0
	for _, group := range groups {
		m := make(map[string]bool)
		for _, person := range group {
			for _, ru := range person {
				ans := string(ru) // annoying type conversion from rune to string
				m[ans] = true
			}
		}
		mLen := 0
		for _, _ = range m {
			mLen += 1
		}
		sum += mLen
	}
	return sum
}

func sol2(groups [][]string) int {
	sum := 0
	for _, group := range groups {
		m := make(map[string]int)
		for _, person := range group {
			for _, ru := range person {
				ans := string(ru) // annoying type conversion from rune to string
				_, ok := m[ans]
				if !ok {
					m[ans] = 1
				} else {
					m[ans] += 1
				}
			}
		}
		mLen := 0
		for k, _ := range m {
			if m[k] == len(group) {
				mLen += 1
			}
		}
		sum += mLen
	}
	return sum
}

func main() {
	in := readInput()
	solpt1 := sol1(in)
	fmt.Printf("The answer to part 1 is: %d\n", solpt1)
	solpt2 := sol2(in)
	fmt.Printf("The answer to part 2 is: %d\n", solpt2)
}
