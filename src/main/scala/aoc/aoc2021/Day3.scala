package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day3 extends Aoc("aoc2021/input3.txt", identity):

  extension (s: String)
    def binaryToInt(): Int = Integer.parseInt(s, 2)

  val n2 = input.size / 2
  val digits = input.head.size
  val zeros = List.fill(digits)(0)
  val counters = input.foldLeft(zeros)((accu, line) => {
    accu.zipWithIndex.map {
      case (c, i) if line(i) == '1' => c + 1
      case (c, _) => c
    }}
  )
  val binary = counters.map(c => if (c > n2) 1 else 0)
  val gamma = binary.mkString.binaryToInt()
  val epsilon = binary.map(b => if (b == 1) 0 else 1).mkString.binaryToInt()
  val res1 = gamma * epsilon
  println(s"res1=$res1")

  def greaterOrEqual(self:Int, that: Int): Boolean = self >= that
  def lessOrEqual(self: Int, that: Int): Boolean = self <= that

  def filter(ix: Int, c: Char, list: List[String], check: (Int, Int) => Boolean): List[String] = {
    val filtered = list.filter(_.charAt(ix) == c)
    if (check(filtered.size, list.size - filtered.size)) filtered else list diff filtered
  }

  @tailrec
  def find(ix: Int, c: Char, list: List[String], check: (Int, Int) => Boolean): String = {
    //println(s"list=$list")
    if ( ix >= digits || list.size <= 1) list.head
    else find(ix + 1, c, filter(ix, c, list, check), check)
  }

  val oxygen = find(0, '1', input, greaterOrEqual)
  val co2 = find(0, '0', input, lessOrEqual)
  val res2 = oxygen.binaryToInt() * co2.binaryToInt()
  println(s"res2=$res2")
