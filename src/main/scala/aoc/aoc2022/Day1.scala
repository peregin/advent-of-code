package aoc.aoc2022

import aoc.Aoc

object Day1 extends aoc.Aoc("aoc2022/input1.txt", identity):

  val cals = splitByEmptyLine(input).map(_.map(_.toLong).sum)

  val res1 = cals.max
  println(s"res1: $res1")

  val res2 = cals.sorted.takeRight(3).sum
  println(s"res2: $res2")



