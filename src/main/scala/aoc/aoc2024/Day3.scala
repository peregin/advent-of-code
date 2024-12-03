package aoc.aoc2024

object Day3 extends aoc.Aoc("aoc2024/input3.txt", identity):

  println("lines=" + input.length) // 🤦‍♂️
  val regex = """mul\(\d{1,3},\d{1,3}\)""".r
  val part1 = input.map(l => regex.findAllIn(l).map{ case s"mul($a,$b)" => a.toInt * b.toInt}.sum).sum
  println(s"part1=$part1")

  val regex2 = """mul\(\d+,\d+\)|don't\(\)|do\(\)""".r

  def process(tokens: List[String], active: Boolean): Long = tokens match {
    case Nil => 0
    case s"mul($i,$j)" :: tail => (if (active) i.toLong * j.toLong else 0) + process(tail, active)
    case "don't()" :: tail => process(tail, active = false)
    case "do()" :: tail => process(tail, active = true)
  }
  val part2 = process(input.flatMap(regex2.findAllMatchIn(_).map(_.matched)), active = true)
  println(s"part2=$part2")
