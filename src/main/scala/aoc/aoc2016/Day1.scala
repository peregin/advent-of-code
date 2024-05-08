package aoc.aoc2016

object Day1 extends App {

  object Coord {
    val zero = Coord(0, 0)
  }

  case class Coord(y: Int, x: Int):
    def +(that: Coord): Coord = Coord(y = this.y + that.y, x = this.x + that.x)

  println(1)
}
