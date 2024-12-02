package aoc.aoc2024

import scala.collection.mutable.ArrayBuffer

object Day2 extends aoc.Aoc("aoc2024/input2.txt", identity):
  val reports = input.map(_.split(' ').map(_.toInt))

  def safe(a: Array[Int]): Boolean = {
    val diffs = a.zip(a.tail).map { case (a, b) => b - a }
    (diffs.forall(_ < 0) || diffs.forall(_ > 0)) && diffs.forall(d => math.abs(d) >=1 && math.abs(d) <= 3)
  }

  val (noBadLevel, unsafe) = reports.partition(safe)
  val part1 = noBadLevel.length
  println(s"part1=$part1")

  def badLevel(a: Array[Int]): Boolean = {
    val oneLess = a.indices.map{i =>
      val m = ArrayBuffer[Int](a:_*)
      m.remove(i)
      m.toArray
    }
    oneLess.exists(safe)
  }
  val oneBadLevel = unsafe.count(badLevel)
  val part2 = part1 + oneBadLevel
  println(s"part2=$part2")
