package aoc.aoc2022

import aoc.Aoc

object Day1 extends aoc.Aoc("aoc2022/input1.txt", identity):

  val cals = splitByEmptyLine(input).map(_.map(_.toLong).sum)

  val max = cals.max
  println(s"max: $max")

  val max3 = cals.sorted.takeRight(3).sum
  println(s"3 max: $max3")



