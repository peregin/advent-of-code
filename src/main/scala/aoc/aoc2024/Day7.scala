package aoc.aoc2024

import aoc.Aoc

object Day7 extends Aoc("aoc2024/input7.txt", identity):

  val list = input.map { case s"$v1: $v2" =>
    val total = v1.toLong
    val nums  = v2.split(" ").map(_.toLong).toList
    (total, nums)
  }

  def validate(total: Long, nums: List[Long], concatenate: Boolean): Boolean = nums match {
    case head :: Nil => head == total
    case head :: tail if head > total => false
    case head :: next :: tail =>
      validate(total, head + next :: tail, concatenate) ||
      validate(total, head * next :: tail, concatenate) ||
      (concatenate && validate(total, s"$head$next".toLong :: tail, true))
    case Nil => false
  }
  def compute(concatenate: Boolean): Long = list.map((total, nums) => (total, validate(total, nums, concatenate))).filter(_._2).map(_._1).sum
  val part1 = compute(false)
  println(part1)
  val part2 = compute(true)
  println(part2)
