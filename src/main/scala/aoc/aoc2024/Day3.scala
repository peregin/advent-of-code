package aoc.aoc2024

object Day3 extends aoc.Aoc("aoc2024/input3.txt", identity):

  val regex = """mul\(\d{1,3},\d{1,3}\)""".r
  val part1 = regex.findAllIn(input.head).map{ case s"mul($a,$b)" => a.toInt * b.toInt}.sum
  // 27201240
  println(s"part1=$part1")
