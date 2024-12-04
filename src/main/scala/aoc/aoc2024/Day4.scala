package aoc.aoc2024

import aoc.Aoc

object Day4 extends Aoc("aoc2024/input4.txt", _.toArray):

  type Grid = Array[Array[Char]]

  val grid: Grid = input.toArray

  case class Coord(y: Int, x: Int):
    def +(that: Coord, f: Int): Coord = Coord(this.y + that.y * f, this.x + that.x * f)

  extension (g: Grid)
    def nx: Int                     = g.head.length
    def ny: Int                     = g.length
    def get(c: Coord): Char         = g(c.y)(c.x)
    def isInside(c: Coord): Boolean = c.y >= 0 && c.y < ny && c.x < nx && c.x >= 0
    def neighbours(c: Coord): Set[Coord] = (for {
      y <- c.y - 1 to c.y + 1 if y >= 0 && y < ny
      x <- c.x - 1 to c.x + 1 if x >= 0 && x < nx
    } yield Coord(y, x)).toSet - c
    def line(from: Coord, dir: Coord, length: Int): String =
      (0 until length).map(ix => from.+(dir, ix)).filter(isInside).map(get).mkString

  val dirs = List(
    Coord(-1, -1), Coord(-1, 0), Coord(-1, 1),
    Coord(0, -1), Coord(0, 1),
    Coord(1, -1), Coord(1, 0), Coord(1, 1)
  )
  val xmas = for {
    y <- 0 until grid.ny
    x <- 0 until grid.nx
    c = Coord(y, x)
    dir <- dirs
    line = grid.line(c, dir, 4)
    //_ = println(s"$c $dir $line")
    if line == "XMAS"
  } yield c
  val part1 = xmas.size
  println(s"part 1: $part1")

  val cross = Set("MAS", "SAM")
  val xmas2 = for {
    y <- 0 until grid.ny
    x <- 0 until grid.nx
    c = Coord(y, x)
    line1 = grid.line(c.+(Coord(-1, -1), 1), Coord(1, 1), 3)
    line2 = grid.line(c.+(Coord(-1, 1), 1), Coord(1, -1), 3)
    if cross.contains(line1) && cross.contains(line2)
  } yield c
  val part2 = xmas2.size
  println(s"part 2: $part2")
