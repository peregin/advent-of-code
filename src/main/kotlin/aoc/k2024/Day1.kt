package aoc.k2024

import aoc.getLines
import kotlin.math.absoluteValue

fun main() {
    val lines = getLines("/aoc2024/input1.txt")
    val (a, b) = lines
        .map { line -> line.split("   ").map { it.toInt() } }
        .map { Pair(it.first(), it.last()) }
        .unzip()
    val part1 = a.sorted().zip(b.sorted()).map{ (it.first - it.second).absoluteValue }.sum()
    println(part1)

    val s2 = b.groupingBy { it }.eachCount()
    val part2 = a.map { it * (s2[it] ?: 0) }.sum()
    println(part2)
}