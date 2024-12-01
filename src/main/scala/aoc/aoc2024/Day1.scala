package aoc.aoc2024

object Day1 extends aoc.Aoc("aoc2024/input1.txt", identity):
  val (a, b) = input.map{case s"$a   $b" => (a.toInt, b.toInt)}.unzip
  val part1 = a.sorted.zip(b.sorted).map((x, y) => (x - y).abs).sum
  println(part1)

  val s2 = b.groupMapReduce(identity)(_ => 1)(_ + _)
  val part2 = a.map(x => x * s2.getOrElse(x, 0)).sum
  println(part2)