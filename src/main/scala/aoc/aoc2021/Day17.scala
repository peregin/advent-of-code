package aoc.aoc2021

import aoc.Aoc

object Day17 extends Aoc("aoc2021/input17.txt", identity):

  type Coord = (Int, Int) // y, x
  type Rectangle = (Coord, Coord)

  def find(s: String): Option[Int] = s match {
    case s"target area: x=$x1..$x2, y=$y1..$y2" =>
      val area = ((y1.toInt, x1.toInt), (y2.toInt, x2.toInt))
      find(area)
    case _ => None
  }

  def find(area: Rectangle): Option[Int] =
    val start = (0, 0)
    val trials = for {
      y <- 0 until 100
      x <- 0 until 100
    } yield (y, x)
    val results = trials.map(t => simulate(start, t, area))
    ???

  // max y as return if was hitting the target area
  def simulate(start: Coord, velocity: Coord, target: Rectangle): Option[Int] = None // TODO: implement



  val res1 = find("target area: x=20..30, y=-10..-5")
    println(s"res1=$res1")








