package aoc.aoc2022

import aoc.Aoc

object Day6 extends aoc.Aoc("aoc2022/input6.txt", identity):

  val message = input(1) // 0 is the example, 1 is the real input
  def marker(s: String, window: Int): Option[(String, Int)] =
    s.sliding(window).zipWithIndex.find(_._1.toSet.size == window).map((s, ix) => (s, ix + window))

  println(s"res1: ${marker(message, 4)}")
  println(s"res2: ${marker(message, 14)}")



