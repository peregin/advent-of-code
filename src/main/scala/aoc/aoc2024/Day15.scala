package aoc.aoc2024

import aoc.Aoc

import scala.annotation.targetName

object Day15 extends Aoc("aoc2024/input15.txt", identity):

  val group = splitByEmptyLine(input)
  type Grid = Array[Array[Char]]
  val grid: Grid = group(0).map(_.toArray).toArray
  val moves      = group(1).flatten

  case class Coord(y: Int, x: Int):
    def op(that: Coord, f: Int): Coord = Coord(this.y + that.y * f, this.x + that.x * f)

    @targetName("plus")
    def +(that: Coord): Coord = op(that, 1)

    @targetName("minus")
    def -(that: Coord): Coord = op(that, -1)

  extension (g: Grid)
    def nx: Int                      = g.head.length
    def ny: Int                      = g.length
    def get(c: Coord): Char          = g(c.y)(c.x)
    def set(c: Coord, v: Char): Grid = {
      g(c.y)(c.x) = v
      g
    }
    def isInside(c: Coord): Boolean  = c.y >= 0 && c.y < ny && c.x < nx && c.x >= 0
    def find(f: Char): Coord = {
      val y = g.indexWhere(_.contains(f))
      val x = g(y).indexWhere(_ == f)
      Coord(y, x)
    }
    def findAll(f: Char): Seq[Coord] =
      for {
        y <- 0 until ny
        x <- 0 until nx
        c = Coord(y, x)
        v = g(y)(x) if v == f
      } yield c
    def show(): Unit = {
      println()
      g.foreach(row =>
        println(row.map {
          case d if d == '.' => Console.YELLOW + d + Console.RESET
          case d if d == '#' => Console.BLUE + d + Console.RESET
          case d if d == 'O' => Console.MAGENTA + d + Console.RESET
          case o             => o
        }.mkString(""))
      )
    }

  def dir(m: Char): Coord = m match {
    case '^' => Coord(-1, 0)
    case 'v' => Coord(1, 0)
    case '<' => Coord(0, -1)
    case '>' => Coord(0, 1)
    case other => throw new Exception(s"Unknown direction $other")
  }

  def step(g: Grid, m: Char): Grid = {
    val c = g.find('@')
    val move = dir(m)
    val next = c + move
    val notBox = Iterator.iterate(next)(_ + move).find(n => g.get(n) != 'O').get
    if (g.get(notBox) == '.')
      g.set(notBox, 'O').set(c, '.').set(next, '@')
    else
      g
  }

  val res = moves.foldLeft(grid)((g, m) => {
    val n = step(g, m)
    //n.show()
    //println(s"move $m")
    n
  })

  res.show()

  def calc(g: Grid): Long = g.findAll('O').map(o => o.y * 100L + o.x).sum

  val part1 = calc(res)
  println(s"part1 $part1")
