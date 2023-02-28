package aoc.aoc2019

import scala.annotation.tailrec

object Day1 extends aoc.Aoc("aoc2019/input1.txt", _.toLong) {

  def fuel(n: Long) = n / 3 - 2
  val res1 = input.map(fuel).sum
  println(res1) // 3360301

  def fuel2(n: Long): Long = {
    val next = fuel(n)
    if (next <= 0) 0 else next + fuel2(next)
  }

  def fuel3(n: Long): LazyList[Long] = {
    val f = fuel(n)
    f #:: fuel3(f)
  }

  //val res2 = input.map(fuel2).sum
  val res2 = input.map(i => fuel3(i).takeWhile(_ > 0).sum).sum
  println(res2) // 5037595
}
