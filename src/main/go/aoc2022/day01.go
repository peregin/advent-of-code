package main

import (
	"log"
	"os"
	"strconv"
	"sort"
	"strings"
)

func sum(arr []int) int {
    sum := 0
    for _, i := range arr {
        sum += i
    }
    return sum
}

func main() {
	input, err := os.ReadFile("../../resources/aoc2022/input1.txt")
	if err != nil {
		log.Fatal(err)
	}
	var calories []int
	elves := strings.Split(string(input), "\n\n")
	for _, elf := range elves {
		//println(elf)
		cals := strings.Split(string(elf), "\n")
		sum := 0
		for _, c := range cals {
			i, _ := strconv.Atoi(c)
			sum += i
		}
		//println(sum)
		calories = append(calories, sum)
	}
	sort.Sort(sort.Reverse(sort.IntSlice(calories)))

	println("part 1: ", calories[0])
	println("part 2: ", sum(calories[0:3]))
}