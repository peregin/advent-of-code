package aoc.aoc2021

import aoc.Aoc
import aoc.aoc2020.Day3.Position

object Day2 extends Aoc("aoc2021/input2.txt", identity) {

  case class Position(x: Int, y: Int) {
    def +(that: Position) = Position(this.x + that.x, this.y + that.y)
  }

  val pos = input.map{
    case s"forward $unit" => Position(unit.toInt, 0)
    case s"down $unit" => Position(0, unit.toInt)
    case s"up $unit" => Position(0, -unit.toInt)
    case other => sys.error(s"unexpected input: $other")
  }.reduce(_ + _)
  val res = pos.x  * pos.y
  println(s"result is $res")
}
