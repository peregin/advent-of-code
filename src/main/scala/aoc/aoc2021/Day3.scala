package aoc.aoc2021

import aoc.Aoc
import aoc.aoc2020.Day3.Position

import scala.annotation.tailrec

object Day3 extends Aoc("aoc2021/input3.txt", identity):

  val n2 = input.size / 2
  val zeros = List.fill(input.head.size)(0)
  val counters = input.foldLeft(zeros)((accu, line) => {
    accu.zipWithIndex.map {
      case (c, i) if line(i) == '1' => c + 1
      case (c, _) => c
    }}
  )
  val binary = counters.map(c => if (c > n2) 1 else 0)
  val gamma = Integer.parseInt(binary.mkString, 2)
  val epsilon = Integer.parseInt(binary.map(b => if (b == 1) 0 else 1).mkString, 2)
  val res = gamma * epsilon
  println(s"res=$res")
