package aoc.k2022

fun main() {
    val groups = getGroupedLines("/aoc2022/input1.txt")
    val calories = groups.map { lines -> lines.sumBy { it.toInt() } }.sortedDescending()
    println("res1: ${calories.max()}")
    println("res2: ${calories.take(3).sum()}")
}