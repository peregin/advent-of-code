package aoc.aoc2022

import aoc.Aoc
import math._

object Day9 extends aoc.Aoc("aoc2022/input9.txt", identity):

  case class Coord(y: Int, x: Int):

    def -(that: Coord): Coord = Coord(this.y - that.y, this.x - that.x)
    def +(that: Coord): Coord = Coord(this.y + that.y, this.x + that.x)

    def next(h: Coord): Coord = {
      val diff = this - h
      val diffX = diff.x.abs
      val diffY = diff.y.abs
      // if adjacent do nothing, otherwise calculate the next step
      if diffX <= 1 && diffY <= 1 then this
      else this - Coord(diff.y.sign, diff.x.sign)
    }

  val zero = Coord(0, 0)
  case class Rope(knots: List[Coord]):
    def next(c: Coord): Rope = {
      val nextHead = knots.head + c
      val newKnots = nextHead +: buildTail(nextHead, knots.tail, Nil)
      Rope(newKnots)
    }

    @annotation.tailrec
    private def buildTail(h: Coord, rest: List[Coord], accu: List[Coord]): List[Coord] =
      if rest.isEmpty then accu else {
        val cur = rest.head
        val next = cur.next(h)
        buildTail(next, rest.tail, accu :+ next)
      }

  val moves = input.flatMap {
    case s"L $m" => List.fill(m.toInt)(Coord(0, -1))
    case s"R $m" => List.fill(m.toInt)(Coord(0, 1))
    case s"U $m" => List.fill(m.toInt)(Coord(1, 0))
    case s"D $m" => List.fill(m.toInt)(Coord(-1, 0))
  }
  @annotation.tailrec
  def move(moves: List[Coord], rope: Rope, accu: List[Rope]): List[Rope] = {
    if moves.isEmpty then accu
    else {
      val next = rope.next(moves.head)
      move(moves.tail, next, next +: accu)
    }
  }

  def debug(points: Set[Coord], nx: Int, ny: Int): Unit = {
    for (cy <- ny to -ny by -1) {
      for (cx <- -nx to nx) {
        val c = Coord(cy, cx)
        print(if points.contains(c) then "#" else ".")
      }
      println()
    }
  }

  val rope1 = Rope(List.fill(2)(zero))
  val all1 = move(moves, rope1, Nil)
  val res1 = all1.map(_.knots.last).distinct.size
  println(s"res1: $res1") // 6269

  val rope2 = Rope(List.fill(10)(zero))
  val all2 = move(moves, rope2, Nil)
  debug(all2.map(_.knots.last).toSet, 15, 15)

  val res2 = all2.map(_.knots.last).distinct.size
  println(s"res2: $res2") // 2557



