import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {

  val grid = Source
    .fromResource("input3.txt")
    .getLines()
    .toList
    .map(_.toList)

  val gridWidth  = grid.head.size
  val gridHeight = grid.size

  case class Position(x: Int, y: Int) {
    def +(that: Position) = Position((this.x + that.x) % gridWidth, this.y + that.y)
    def outsideTheGrid    = y >= gridHeight
  }
  val start = Position(0, 0)

  def count(from: Position, step: Position): Long = {
    @tailrec
    def move(from: Position, step: Position, path: List[Char]): List[Char] =
      if (from.outsideTheGrid) path else move(from + step, step, path :+ grid(from.y)(from.x))
    move(from, step, List.empty).count(_ == '#')
  }

  val solution1 = count(start, Position(3, 1))
  println(s"solution1 = $solution1")

  val solution2 =
    List(Position(1, 1), Position(3, 1), Position(5, 1), Position(7, 1), Position(1, 2)).map(count(start, _)).product
  println(s"solution2 = $solution2")
}
