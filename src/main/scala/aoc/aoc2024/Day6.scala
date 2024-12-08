package aoc.aoc2024

import aoc.Aoc

import scala.annotation.targetName

object Day6 extends Aoc("aoc2024/input6.txt", _.toArray):

  type Grid = Array[Array[Char]]

  val grid: Grid = input.toArray

  case class Coord(y: Int, x: Int):
    def op(that: Coord, f: Int): Coord = Coord(this.y + that.y * f, this.x + that.x * f)
    @targetName("plus")
    def +(that: Coord): Coord = op(that, 1)
    @targetName("minus")
    def -(that: Coord): Coord = op(that, -1)
    def rotateLeft(): Coord   = Coord(-x, y)
    def rotateRight(): Coord  = Coord(x, -y)

  extension (g: Grid)
    def nx: Int                      = g.head.length
    def ny: Int                      = g.length
    def get(c: Coord): Char          = g(c.y)(c.x)
    def set(c: Coord, v: Char): Unit = g(c.y)(c.x) = v
    def isInside(c: Coord): Boolean  = c.y >= 0 && c.y < ny && c.x < nx && c.x >= 0
    def find(f: Char): Coord = {
      val y = g.indexWhere(_.contains(f))
      val x = g(y).indexWhere(_ == f)
      Coord(y, x)
    }
    def findAll(f: Char): Seq[Coord] =
      (for {
        y <- 0 until ny
        x <- 0 until nx
        c = Coord(y, x)
        v = g(y)(x) if v == f
      } yield c)
    def show(): Unit = {
      println()
      g.foreach(row =>
        println(row.map {
          case d if d == '.' => Console.YELLOW + d + Console.RESET
          case o             => o
        }.mkString(""))
      )
    }

  def guard(c: Coord, d: Coord, accu: Set[Coord]): Set[Coord] = {
    val next = c + d
    if (!grid.isInside(next)) accu
    else if (grid.get(next) == '#') guard(c, d.rotateRight(), accu)
    else guard(next, d, accu + next)
  }

  grid.show()
  val start = grid.find('^')
  println(s"start=$start")
  val dir  = Coord(-1, 0) // up
  val path = guard(start, dir, Set(start))
  // path.foreach(c => grid.set(c, 'X'))
  // grid.show()
  println(s"path=${path.size}")

  case class Pair(c: Coord, d: Coord)
  def loop(c: Pair, accu: Set[Pair]): Boolean = {
    if (accu.contains(c)) {
//      println(s"loop detected at $c")
//      println(s"${accu.mkString(",")}")
      true
    }
    else {
      val next = c.c + c.d
      if (!grid.isInside(next)) false
      else {
        val f = grid.get(next)
        if (f == '#' || f == 'O') loop(Pair(c.c, c.d.rotateRight()), accu + c)
        else loop(Pair(next, c.d), accu + c)
      }
    }
  }

  //  grid.set(start + Coord(0, -1), 'O')
  //  grid.show()
  //  val res = loop(s1, Set.empty)
  //  println(s"res=$res")

  val s1 = Pair(start, dir)
  val part2 = grid.findAll('.').count(c =>
    grid.set(c, 'O')
    val res = loop(s1, Set.empty)
    grid.set(c, '.')
    res
  )
  println(s"part2=$part2")

