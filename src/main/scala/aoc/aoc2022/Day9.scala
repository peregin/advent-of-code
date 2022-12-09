package aoc.aoc2022

import aoc.Aoc
import math._

object Day9 extends aoc.Aoc("aoc2022/input9.txt", identity):

  case class Coord(y: Int, x: Int):
    def nextHead(c: Coord): Coord = Coord(this.y + c.y, this.x + c.x)

    def nextTail(h: Coord): Coord = {
      val diffX = abs(this.x - h.x)
      val diffY = abs(this.y - h.y)
      if diffX > 1 && diffY > 1 then Coord(this.y - signum(this.y - h.y), this.x - signum(this.x - h.x))
      else if diffX > 1 then Coord(h.y, this.x - signum(this.x - h.x))
      else if diffY > 1 then Coord(this.y - signum(this.y - h.y), h.x)
      else this
    }

  val zero = Coord(0, 0)
  case class Rope(head: Coord, tail: List[Coord]):
    def next(c: Coord): Rope = {
      val nextHead = head.nextHead(c)
      val nextTail = buildTail(nextHead, tail, Nil)
      Rope(nextHead, nextTail)
    }

    @annotation.tailrec
    private def buildTail(h: Coord, rest: List[Coord], accu: List[Coord]): List[Coord] =
      if rest.isEmpty then accu else {
        val cur = rest.head
        val next = cur.nextTail(h)
        buildTail(next, rest.tail, accu :+ next)
      }

    def coords(): Set[Coord] = (head +: tail).toSet

  val moves = input.flatMap {
    case s"L $m" => List.fill(m.toInt)(Coord(0, -1))
    case s"R $m" => List.fill(m.toInt)(Coord(0, 1))
    case s"U $m" => List.fill(m.toInt)(Coord(1, 0))
    case s"D $m" => List.fill(m.toInt)(Coord(-1, 0))
  }
  val rope = Rope(zero, List.fill(9)(zero))

  @annotation.tailrec
  def move(moves: List[Coord], rope: Rope, accu: List[Rope]): List[Rope] = {
    if moves.isEmpty then accu
    else {
      val next = rope.next(moves.head)
      move(moves.tail, next, next +: accu)
    }
  }
  val all = move(moves, rope, Nil)

  //val res1 = all.map(_.tail).distinct.size
  //println(s"res1: $res1") // 6269

  def debug(points: Set[Coord], nx: Int, ny: Int): Unit = {
    for (cy <- ny to -ny by -1) {
      for (cx <- -nx to nx) {
        val c = Coord(cy, cx)
        print(if points.contains(c) then "#" else ".")
      }
      println()
    }
  }
  debug(all.map(_.tail.last).toSet, 15, 15)

  val res2 = all.map(_.tail.last).toSet.size
  println(s"res2: $res2") // 2557



