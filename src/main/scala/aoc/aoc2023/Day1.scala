package aoc.aoc2023

import aoc.Aoc

object Day1 extends aoc.Aoc("aoc2023/input1.txt", identity):

  val digits = List(
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
  )

  def convert(line: String): String =
    val firstDigit = line.find(_.isDigit).map(c => (line.indexOf(c), c.asDigit - 1)).getOrElse((-1, -1))
    val first      = (firstDigit +: digits.map(line.indexOf).zipWithIndex).filter(_._1 > -1).minBy(_._1)

    val lastDigit = line.findLast(_.isDigit).map(c => (line.lastIndexOf(c), c.asDigit - 1)).getOrElse((-1, -1))
    val last      = (lastDigit +: digits.map(line.lastIndexOf).zipWithIndex).filter(_._1 > -1).maxBy(_._1)
    s"${first._2 + 1}${last._2 + 1}"

  def num(s: String): Int =
    val d = s.filter(_.isDigit)
    d.head.asDigit * 10 + d.last.asDigit

  def calibrate(c: String => String): Int = input.map(c).map(num).sum

  val res1 = calibrate(identity)
  println(s"res1: $res1") // 55712

  // eighthree = 83 🤦‍
  val res2 = calibrate(convert) //55413
  println(s"res2: $res2")
