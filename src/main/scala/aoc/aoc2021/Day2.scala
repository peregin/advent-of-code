package aoc.aoc2021

import aoc.Aoc
import aoc.aoc2021.Day2.{input, pos}

case class Position(x: Int, y: Int, aim: Int = 0) {
  def +(that: Position) = Position(this.x + that.x, this.y + that.y, this.aim + that.aim)
}

object Day2 extends Aoc("aoc2021/input2.txt", identity) {

  val pos = input.map{
    case s"forward $unit" => Position(unit.toInt, 0)
    case s"down $unit" => Position(0, unit.toInt)
    case s"up $unit" => Position(0, -unit.toInt)
    case other => sys.error(s"unexpected input: $other")
  }.reduce(_ + _)
  val res = pos.x  * pos.y
  println(s"result is $res")
}

object Day2_2 extends Aoc("aoc2021/input2_2.txt", identity) {

  val pos = input.foldLeft(Position(0, 0, 0))( (pos, instr) => {
    instr match {
      case s"forward $unit" => pos + Position(unit.toInt, unit.toInt * pos.aim)
      case s"down $unit" => pos + Position(0, 0, unit.toInt)
      case s"up $unit" => pos + Position(0, 0, -unit.toInt)
      case other => sys.error(s"unexpected input: $other")
    }
  })
  val res = pos.x  * pos.y
  println(s"result is $res")
}
