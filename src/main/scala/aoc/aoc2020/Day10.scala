package aoc.aoc2020

import aoc.Aoc

object Day10 extends Aoc("aso2020/input10.txt", _.toLong) {

  val sorted = input.sorted
  val diffs  = (sorted :+ (sorted.max + 3L)).zip(0L +: sorted).map(t => t._1 - t._2)
  println(diffs)
  val solution1 = diffs.count(_ == 1) * diffs.count(_ == 3)
  println(s"solution1=$solution1")

  // for consecutive 1s
  def conns(n: Long): Long =
    n match {
      case zeroOrNegative if zeroOrNegative <= 0L => 0L
      case 1                                      => 1L
      case 2                                      => 1L
      case 3                                      => 2L
      case _                                      => conns(n - 1) + conns(n - 2) + conns(n - 3)
    }

  // group the list with the consequtive elements
  def group[T](list: List[T]): List[List[T]] =
    list match {
      case Nil => Nil
      case h :: t =>
        val segment = list takeWhile { h == _ }
        segment :: group(list drop segment.length)
    }

  //conns(4) * conns(3) for the first example
  val solution2 = group(diffs)
    .map(list =>
      list(0) match {
        case 1 => conns(list.size + 1)
        case _ => 1
      }
    )
    .product
  println(s"solution2=$solution2")
}
