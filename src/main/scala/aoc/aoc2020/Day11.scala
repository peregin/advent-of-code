package aoc.aoc2020

import aoc.Aoc

import scala.annotation.tailrec

object Day11 extends Aoc("aso2020/input11.txt", identity) {

  val gridWidth  = input.head.size
  val gridHeight = input.size

  val FLOOR    = '.'
  val EMPTY    = 'L'
  val OCCUPIED = '#'

  case class Position(x: Int, y: Int) {
    def +(that: Position)       = Position(this.x + that.x, this.y + that.y)
    def insideGrid()            = y >= 0 && y < gridHeight && x < gridWidth && x >= 0
    def outsideGrid()           = !insideGrid()
    def get(grid: List[String]) = grid(y)(x)
  }
  val adjacent =
    List(
      Position(-1, -1),
      Position(-1, 0),
      Position(-1, 1),
      Position(0, 1),
      Position(1, 1),
      Position(1, 0),
      Position(1, -1),
      Position(0, -1)
    )

  def next(curr: List[String], fun: (List[String], Position) => Char): List[String] = {
    val positions = for (y <- 0 until gridHeight; x <- 0 until gridWidth) yield Position(x, y)
    positions.map(fun(curr, _)).sliding(gridWidth, gridWidth).map(_.mkString).toList
  }

  @tailrec
  def find(curr: List[String], fun: (List[String], Position) => Char): Int = {
    val candidate = next(curr, fun)
    if (curr == candidate) curr.map(_.count(_ == OCCUPIED)).sum else find(candidate, fun)
  }

  val solution1 = find(
    input,
    (curr, p) => {
      val a = adjacent.map(_ + p).filter(_.insideGrid()).map(_.get(curr)).filterNot(_ == FLOOR)
      p.get(curr) match {
        case FLOOR                                   => FLOOR
        case EMPTY if a.forall(_ == EMPTY)           => OCCUPIED
        case OCCUPIED if a.count(_ == OCCUPIED) >= 4 => EMPTY
        case other                                   => other
      }
    }
  )
  println(s"solution1 = $solution1")

  @tailrec
  def adj(curr: List[String], p: Position, dir: Position): Position =
    if (!p.insideGrid()) p
    else {
      val c = p.get(curr)
      if (c == OCCUPIED || c == EMPTY) p
      else adj(curr, p + dir, dir)
    }

  val solution2 = find(
    input,
    (curr, p) => {
      val a = adjacent.map(a => adj(curr, p + a, a)).filter(_.insideGrid()).map(_.get(curr)).filterNot(_ == FLOOR)
      p.get(curr) match {
        case FLOOR                                   => FLOOR
        case EMPTY if a.forall(_ == EMPTY)           => OCCUPIED
        case OCCUPIED if a.count(_ == OCCUPIED) >= 5 => EMPTY
        case other                                   => other
      }
    }
  )
  println(s"solution1 = $solution2")
}
