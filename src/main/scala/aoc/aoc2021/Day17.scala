package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day17 extends Aoc("aoc2021/input17.txt", identity):

  object Coord {
    val zero = Coord(0, 0)
  }
  case class Coord(y: Int, x: Int):
    def +(that: Coord): Coord = Coord(y = this.y + that.y, x = this.x + that.x)

  case class Rectangle(corner1: Coord, corner2: Coord):
    val minX: Int = corner1.x min corner2.x
    val maxX: Int = corner1.x max corner2.x
    val minY: Int = corner1.y min corner2.y
    val maxY: Int = corner1.y max corner2.y
    def contains(c: Coord): Boolean = minY <= c.y && c.y <= corner2.y && minX <= c.x && c.x <= corner2.x

  def findY(s: String): Seq[Int] =
    println(s"target area: $s")
    s match {
    case s"target area: x=$x1..$x2, y=$y1..$y2" =>
      val area = Rectangle(
        corner1 = Coord(y = y1.toInt, x = x1.toInt),
        corner2 = Coord(y = y2.toInt, x = x2.toInt))
      trialsForY(area)
    case _ => Nil
  }

  def trialsForY(area: Rectangle): Seq[Int] =
    val n = 300
    for {
      y <- -n to n
      x <- -n to n
      s <- simulateMaxY(Coord.zero, Coord(y = y, x = x), area)
    } yield s

  // max y as return if was hitting the target area
  def simulateMaxY(start: Coord, velocity: Coord, target: Rectangle): Option[Int] =
    @tailrec
    def step(from: Coord, velocity: Coord, maxY: Int): Option[Int] =
      //println(s"$from, $velocity, $maxY")
      if target.contains(from) then Some(maxY)
      else if target.minY > from.y || target.maxX < from.x then None
      else
        val newStart = from + velocity
        val newVelocity = velocity + Coord(y = -1, x = -Integer.signum(velocity.x))
        step(newStart, newVelocity, maxY max from.y)
    step(start, velocity, start.y)


  //val res1 = find("target area: x=20..30, y=-10..-5")
  val res = findY(input.head)
  // 8911
  val res1 = res.max
  println(s"res1=$res1")
  // 4748
  val res2 = res.size
  println(s"res2=$res2")








