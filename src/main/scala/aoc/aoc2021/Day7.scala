package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day7 extends Aoc("aoc2021/input7.txt", identity):

  val scrabs = input.head.split(',').map(_.toInt).sorted
  val median = scrabs(scrabs.length / 2)
  val res1 = scrabs.map(s => (s - median).abs).sum
  println(s"res1=$res1")


