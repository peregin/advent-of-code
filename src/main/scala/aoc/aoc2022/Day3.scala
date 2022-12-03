package aoc.aoc2022

import aoc.Aoc

object Day3 extends aoc.Aoc("aoc2022/input3.txt", identity):

  def convert(c: Char): Int = c match
    case lower if lower.isLower => lower.toInt - 96
    case upper => upper.toInt - 64 + 26

  val res = input.map{ line =>
    val half = line.length / 2
    val a = line.take(half).toSet
    val b = line.drop(half).toSet
    a.intersect(b).head
  }.map(convert).sum

  println(s"sum1: $res")

  val res2 = input.sliding(3, 3).toList.map{group =>
    group.map(_.toSet).reduce(_ intersect _).head
  }.map(convert).sum
  println(s"sum2: $res2")




