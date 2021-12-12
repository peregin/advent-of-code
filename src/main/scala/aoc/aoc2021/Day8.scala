package aoc.aoc2021

import aoc.Aoc

object Day8 extends Aoc("aoc2021/input8.txt", identity):

  case class SegmentOutput(in: List[String], out: List[String])

  val so = input.map{
    case s"$in | $out" => SegmentOutput(in.split(" ").toList, out.split(" ").toList)
  }

  val digits = List(
    "abcefg",
    "cf",
    "acdeg",
    "acdfg",
    "bcdf",
    "abdfg",
    "abdefg",
    "acf",
    "abcdefg",
    "abcdfg",
  ).map(_.toSet)

  val known = digits.map(_.size).groupMapReduce(identity)(_ => 1)(_ + _).filter(_._2 == 1).keySet

  val res1 = so.map(_.out).flatMap(_.map(_.size)).count(known.contains)
  println(s"res1=$res1")




