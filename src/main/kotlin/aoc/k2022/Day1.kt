package aoc.k2022

fun main() {
    val lines = object{}.javaClass.getResource("/aoc2022/input1.txt")?.readText()
    val groups = lines?.split("\n\n")?.map { it.lines() }
    val calories = groups?.map { lines -> lines.sumBy { it.toInt() } }?.sortedDescending()
    println("res1: ${calories?.max()}")
    println("res2: ${calories?.take(3)?.sum()}")
}