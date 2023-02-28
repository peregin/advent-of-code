package aoc.aoc2017

object Day1 extends aoc.Aoc("aoc2017/input1.txt", identity) {

  val in = input.head
  val res1 = (in :+ in.head).sliding(2).filter(f => f.charAt(0) == f.charAt(1)).map(_.charAt(0).asDigit).sum
  println(res1)
}
