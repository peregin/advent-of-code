package aoc.aoc2022

import aoc.Aoc
import math._

object Day9 extends aoc.Aoc("aoc2022/input9sample2.txt", identity):

  case class Coord(y: Int, x: Int):
    def nextHead(c: Coord): Coord = Coord(this.y + c.y, this.x + c.x)

    def nextTail(h: Coord): Coord =
      if abs(this.x - h.x) > 1 then Coord(h.y, this.x - signum(this.x - h.x))
      else if abs(this.y - h.y) > 1 then Coord(this.y - signum(this.y - h.y), h.x)
      else this

  val zero = Coord(0, 0)
  case class Rope(head: Coord, tail: Coord):
    def next(c: Coord): Rope = {
      val nextHead = head.nextHead(c)
      val nextTail = tail.nextTail(nextHead)
      Rope(nextHead, nextTail)
    }

  val moves = input.flatMap {
    case s"L $m" => List.fill(m.toInt)(Coord(0, -1))
    case s"R $m" => List.fill(m.toInt)(Coord(0, 1))
    case s"U $m" => List.fill(m.toInt)(Coord(1, 0))
    case s"D $m" => List.fill(m.toInt)(Coord(-1, 0))
  }
  val rope = Rope(zero, zero)

  @annotation.tailrec
  def move(moves: List[Coord], rope: Rope, accu: List[Rope]): List[Rope] = {
    if moves.isEmpty then accu
    else {
      val next = rope.next(moves.head)
      move(moves.tail, next, next +: accu)
    }
  }
  val all = move(moves, rope, Nil)
  //all.foreach(println)

  val res1 = all.map(_.tail).distinct.size
  println(s"res1: $res1") // 6269

  case class Rope2(head: Coord, tail: List[Coord])
  val rope2 = Rope2(zero, List.fill(9)(zero))

  val res2 = rope2
  println(s"res2: $res2")



