package aso.aso2020

import aso.Aso

import scala.annotation.tailrec

object Day3 extends Aso("aso2020/input3.txt", identity) {

  val gridWidth  = input.head.size
  val gridHeight = input.size

  case class Position(x: Int, y: Int) {
    def +(that: Position) = Position((this.x + that.x) % gridWidth, this.y + that.y)
    def outsideTheGrid    = y >= gridHeight
  }
  val start = Position(0, 0)

  def count(from: Position, step: Position): Long = {
    @tailrec
    def move(from: Position, step: Position, path: List[Char]): List[Char] =
      if (from.outsideTheGrid) path else move(from + step, step, path :+ input(from.y)(from.x))
    move(from, step, List.empty).count(_ == '#')
  }

  val solution1 = count(start, Position(3, 1))
  println(s"solution1 = $solution1")

  val solution2 =
    List(Position(1, 1), Position(3, 1), Position(5, 1), Position(7, 1), Position(1, 2)).map(count(start, _)).product
  println(s"solution2 = $solution2")
}
