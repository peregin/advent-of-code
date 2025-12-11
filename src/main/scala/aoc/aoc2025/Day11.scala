package aoc.aoc2025

object Day11 extends aoc.Aoc("aoc2025/input11.txt", identity):

  val graph: Map[String, Seq[String]] = input.map { line =>
    val Array(name, outputs) = line.split(": ")
    name -> outputs.split(" ").toSeq
  }.toMap

  def countPaths(from: String, to: String): Int =
    if (from == to) 1
    else graph.get(from).map(_.map(countPaths(_, to)).sum).getOrElse(0)

  val part1 = countPaths("you", "out")
  println(s"part1=$part1") // 5, 749
