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

type PasswordRule struct {
	Letter   string
	Password string
	Min      int
	Max      int
}

func readInput() []PasswordRule {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal("no input file provided!")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	rules := make([]PasswordRule, 0)
	for scanner.Scan() {
		line := scanner.Text()
		grps := strings.Split(line, " ")
		exp := regexp.MustCompile(`\d+`)
		nums := exp.FindAll([]byte(grps[0]), -1)

		rl := PasswordRule{}
		min, _ := strconv.Atoi(string(nums[0]))
		max, _ := strconv.Atoi(string(nums[1]))
		rl.Min = min
		rl.Max = max
		rl.Letter = string(grps[1][0])
		rl.Password = grps[2]

		rules = append(rules, rl)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	return rules
}

func sol1(rules []PasswordRule) int {
	total := 0
	for _, rule := range rules {
		sum := 0
		for _, ch := range rule.Password {
			if string(ch) == rule.Letter {
				sum += 1
			}
		}
		if sum >= rule.Min && sum <= rule.Max {
			total += 1
		}
	}
	return total
}

func sol2(rules []PasswordRule) int {
	total := 0
	for _, rule := range rules {
		min := false
		max := false
		// I am not proud of this but gotta type convert the rune and then the byte
		min = (string(string(rule.Password)[rule.Min-1]) == rule.Letter)
		max = (string(string(rule.Password)[rule.Max-1]) == rule.Letter)
		if (min || max) && !(min && max) {
			total += 1
		}
	}
	return total
}

func main() {
	rules := readInput()
	total1 := sol1(rules)
	fmt.Printf("total valid passwords for part 1 is: %d!\n", total1)
	total2 := sol2(rules)
	fmt.Printf("total valid passwords for part 2 is: %d!\n", total2)
}
