package aoc.aoc2023

import aoc.Aoc

object Day15 extends aoc.Aoc("aoc2023/input15.txt", identity):

  def hash(s: String): BigInt = s.foldLeft(BigInt(0))((accu, c) => ((accu + c.toInt) * 17) % 256)

  //val hashes = List("HASH").map(hash)
  val hashes = input.head.split(',').map(hash)
  val res1 = hashes.sum
  println(s"res1: $res1")

  val res2 = 2
  println(s"res2: $res2")
