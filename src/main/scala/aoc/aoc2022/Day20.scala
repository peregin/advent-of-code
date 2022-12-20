package aoc.aoc2022

import aoc.Aoc

object Day20 extends aoc.Aoc("aoc2022/input20.txt", identity):

  val file = """1
    |2
    |-3
    |3
    |-2
    |0
    |4""".stripMargin.split('\n').map(_.toInt).toList
  // val initial = input.map(_.toInt)
  val cb = collection.mutable.ListBuffer[Int]()
  cb.addAll(file)

  // num to index
  val start = file.zipWithIndex.toMap

  // iterate and update indixes for every (or most of it) number

  val res1 = start
  println(s"res1: $res1")
