package aoc.k2022

fun getText(resourceName: String): String = object {}.javaClass.getResource(resourceName)!!.readText()
fun getLines(resourceName: String): List<String> = getText(resourceName).lines()
fun getGroupedLines(resourceName: String): List<List<String>> = getText(resourceName).split("\n\n").map { it.lines() }