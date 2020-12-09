package main

import (
	//"bufio"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// As a note, I really hated this one, the cleanliness of the code
// probably shows that :D

var requiredFields = []string{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

type Passport struct {
	Attributes map[string]string
}

func readInput() []*Passport {
	// Our return value is a slice of passports
	passports := make([]*Passport, 0)

	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal("no input file provided!")
	}
	defer file.Close()
	// For this puzzle, I'm embarrassed, but I'm reading
	// the input in as a single buffer because it's easier
	// to split on
	inBytes, err := ioutil.ReadAll(file)
	if err != nil {
		log.Fatal(err)
	}
	inString := string(inBytes)
	ports := strings.Split(inString, "\n\n")
	for _, port := range ports {
		passport := &Passport{}
		passport.Attributes = make(map[string]string)
		re := regexp.MustCompile(`([\w,\d,:,#]+)`)
		fields := re.FindAllString(port, -1)
		for _, field := range fields {
			kv := strings.Split(field, ":")
			key := kv[0]
			val := kv[1]
			passport.Attributes[key] = val
		}
		passports = append(passports, passport)
	}

	return passports
}

func sol1(passports []*Passport) int {
	total := 0
	for _, passport := range passports {
		valid := true
		for _, field := range requiredFields {
			_, ok := passport.Attributes[field]
			if !ok {
				valid = false
				break
			}
		}
		if valid {
			total += 1
		}
	}
	return total
}

func sol2(passports []*Passport) int {
	total := 0
	for _, passport := range passports {
		valid := isValidPassport(passport)
		if valid {
			total += 1
		}
	}
	return total

}

func numDigits(i int) int {
	digits := 0
	for i > 0 {
		i = i / 10
		digits += 1
	}
	return digits
}

func contains(slc []string, s string) bool {
	for _, el := range slc {
		if s == el {
			return true
		}
	}
	return false
}

func isValidPassport(passport *Passport) bool {
	for _, k := range requiredFields {
		v, ok := passport.Attributes[k]
		if !ok {
			return false
		}
		switch k {
		case "byr":
			yr, err := strconv.Atoi(v)
			if err != nil || yr > 2002 || yr < 1920 {

				return false
			}
		case "iyr":
			yr, err := strconv.Atoi(v)
			if err != nil || yr > 2020 || yr < 2010 {
				return false
			}
		case "eyr":
			yr, err := strconv.Atoi(v)
			if err != nil || yr > 2030 || yr < 2020 {
				return false
			}
		case "hgt":
			re := regexp.MustCompile(`[0-9]+`)
			cl := v
			if strings.Contains(cl, "in") {
				heightS := re.FindString(cl)
				height, err := strconv.Atoi(heightS)
				if err != nil {
					return false
				}
				if 59 > height || height > 76 {
					return false
				}
			} else if strings.Contains(cl, "cm") {
				heightS := re.FindString(cl)
				height, err := strconv.Atoi(heightS)
				if err != nil {
					return false
				}
				if 150 > height || height > 193 {
					return false
				}
			} else {
				return false
			}
		case "hcl":
			re := regexp.MustCompile(`#[a-f0-9]+`)
			cl := v
			if len(cl) != 7 || !re.MatchString(cl) {
				return false
			}
		case "ecl":
			cl := v
			if !contains([]string{"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}, cl) {
				return false
			}
		case "pid":
			_, err := strconv.Atoi(v)
			if err != nil || len(v) != 9 {
				return false
			}
		}
	}

	return true
}

func main() {
	in := readInput()
	solpt1 := sol1(in)
	fmt.Printf("The answer to part 1 is: %d\n", solpt1)
	solpt2 := sol2(in)
	fmt.Printf("The answer to part 2 is: %d\n", solpt2)
}
