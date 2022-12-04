package aoc.aoc2022

import aoc.Aoc

object Day4 extends aoc.Aoc("aoc2022/input4.txt", identity):

  case class Section(from: Int, to: Int) {
    require(from <= to, "invalid section definition")

    def fullyContain(that: Section): Boolean = this.from <= that.from && this.to >= that.to

    def overlap(that: Section): Boolean = this.from <= that.from && this.to >= that.from
  }

  val pairs = input.map {
    case s"$f1-$t1,$f2-$t2" =>
      val s1 = Section(f1.toInt, t1.toInt)
      val s2 = Section(f2.toInt, t2.toInt)
      (s1, s2)
  }

  val res1 = pairs.map{
    case (s1, s2) if s1.fullyContain(s2) || s2.fullyContain(s1) => 1
    case _ => 0
  }.sum
  println(s"res1: $res1")

  val res2 = pairs.map {
    case (s1, s2) if s1.overlap(s2) || s2.overlap(s1) => 1
    case _ => 0
  }.sum
  println(s"res2: $res2")



