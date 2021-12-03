package aoc.aoc2021

import aoc.Aoc
import aoc.aoc2020.Day3.Position

import scala.annotation.tailrec

object Day1 extends Aoc("aoc2021/input1.txt", _.toInt):

  def count(prev: Int, list: List[Int]): Int =
    list match {
      case Nil => 0
      case x :: xs =>
        val res = if (x > prev) 1 else 0
        res + count(x, xs)
    }

  val res1 = count(Int.MaxValue, input)
  println(s"res1=$res1")

  val input2 = input.sliding(3).map(_.sum).toList
  val res2 = count(Int.MaxValue, input2)
  println(s"res2=$res2")
