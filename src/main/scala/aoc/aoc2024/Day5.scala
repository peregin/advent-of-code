package aoc.aoc2024

import aoc.Aoc

import scala.annotation.tailrec

object Day5 extends Aoc("aoc2024/input5.txt", identity):

  val orderings = input.takeWhile(_.nonEmpty).map { case s"$a|$b" => (a.toInt, b.toInt) }
  println(s"orderins:\n$orderings")

  val updates = input.drop(orderings.size + 1).map(_.split(",").map(_.toInt).toList)
  // println(s"updates:\n${updates.mkString("\n")}")

  def validated(ordering: (Int, Int), list: List[Int]): Boolean = {
    val indices = ordering.toList.map(list.indexOf(_))
    indices.contains(-1) || indices == indices.sorted
  }

  val (good, bad) = updates.partition(list => orderings.forall(validated(_, list)))
  val part1       = good.map(list => list(list.length / 2)).sum
  println(s"part1=$part1")

  def minOf(up: List[Int]): Int = up.find(e => up.filter(_ != e).forall(other => !orderings.contains((other, e)))).get
  @tailrec
  def sort(up: List[Int], sorted: List[Int] = Nil): List[Int] =
    if (up.isEmpty) sorted
    else {
      val min = minOf(up)
      sort(up.filter(_ != min), sorted ++ List(min))
    }

  val part2 = bad.map(sort(_, Nil)).map(list => list(list.length / 2)).sum
  println(s"part2=$part2")
