package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day6 extends Aoc("aoc2021/input6.txt", identity):

  val initialAges = input.head.split(',').map(_.toInt).toList
  // age -> amount of fishes with the same age - or maybe store in a vector
  type Fishes = Map[Int, Long]
  val init: Fishes = initialAges.groupMapReduce(identity)(_ => 1L)(_ + _)

  def step(in: Fishes): Fishes = in.map {
    case (age, amount) if age == 0 => List((6, amount), (8, amount))
    case (age, amount) => List((age - 1, amount))
  }.flatten.groupMapReduce(_._1)(_._2)(_ + _)

  @tailrec
  def loop(days: Int, in: Fishes): Fishes = {
    if (days == 0) in
    else loop(days - 1, step(in))
  }

  val res1 = loop(80, init).values.sum
  println(s"res1=$res1")

  // 26984457539
  val res2 = loop(256, init).values.sum
  println(s"res2=$res2")

