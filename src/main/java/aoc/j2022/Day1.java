package aoc.j2022;

import java.io.*;
import java.util.Comparator;

import static aoc.Loader.*;

public class Day1 {

    public static void main(String... args) {
        var groups = getGroupedLines("/aoc2022/input1.txt");
        var calories = groups.stream().map(g -> g.stream().mapToInt(Integer::parseInt).sum()).sorted(Comparator.reverseOrder()).toList();
        System.out.println("res1: "+calories.stream().max(Integer::compare).orElse(0));
        System.out.println("res2: " + calories.stream().limit(3).reduce((a, b) -> a + b).orElseThrow());
    }
}
