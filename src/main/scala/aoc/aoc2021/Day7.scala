package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day7 extends Aoc("aoc2021/input7.txt", identity):

  val scrabs = input.head.split(',').map(_.toInt).sorted
  val median = scrabs(scrabs.length / 2)

  def fuel(ref: Int, calc: (Int, Int) => Int) = scrabs.map(s => calc(s, ref)).sum
  val res1 = fuel(median, (ref: Int, on: Int) => (ref - on).abs)
  println(s"res1=$res1")

  val res2 = scrabs.map(s => fuel(s, (ref: Int, on: Int) => {
    val delta = (ref - on).abs
    (delta * (delta + 1)) / 2 // series 1,2,..,n,n+1
  })).min
  println(s"res2=$res2")


