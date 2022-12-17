package aoc.aoc2022

import aoc.Aoc

object Day17 extends aoc.Aoc("aoc2022/input17sample.txt", identity):


  case class Coord(x: Int, y: Int):
    def -(that: Coord): Coord = Coord(this.x - that.x, this.y - that.y)

    def +(that: Coord): Coord = Coord(this.x + that.x, this.y + that.y)
  end Coord

  enum Jet:
    case Left, Right

  val jet = input.head.toCharArray.map{
    case '<' => Jet.Left
    case '>' => Jet.Right
  }.toList

  val res1 = jet
  println(s"res1: $res1")


