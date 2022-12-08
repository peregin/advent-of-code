package aoc.aoc2022

import aoc.Aoc
import aoc.aoc2021.Day15.{Coord, Grid}

object Day8 extends aoc.Aoc("aoc2022/input8.txt", identity):

  type Grid = Array[Array[Int]]

  case class Coord(y: Int, x: Int)

  extension (g: Grid)

    def nx: Int = g.head.size
    def ny: Int = g.size
    def show(): Unit = {
      println()
      g.foreach(row => println(row.mkString(" ")))
    }

    def get(c: Coord): Int = g(c.y)(c.x)

    def isEdge(c: Coord): Boolean = c.x == 0 || c.x == nx - 1 || c.y == 0 || c.y == ny - 1

    def isTallest(c: Coord): Boolean = {
      val left = (0 until c.x).map(Coord(c.y, _)).map(get)
      val right = (c.x + 1 until nx).map(Coord(c.y, _)).map(get)
      val top = (0 until c.y).map(Coord(_, c.x)).map(get)
      val bottom = (c.y + 1 until ny).map(Coord(_, c.x)).map(get)
      val dirs = List(left, right, top, bottom)
      dirs.exists(line => get(c) > line.max)
    }

    def isVisible(c: Coord): Boolean = isEdge(c) || isTallest(c)

    def count(): Int = (for {
      y <- 0 until ny
      x <- 0 until nx
      c = Coord(y, x)
      if isVisible(c)
    } yield c).size

    def score(c: Coord): Int = if isEdge(c) then 0 else {
      val left = (0 until c.x).map(Coord(c.y, _)).map(get).reverse
      val right = (c.x + 1 until nx).map(Coord(c.y, _)).map(get)
      val top = (0 until c.y).map(Coord(_, c.x)).map(get).reverse
      val bottom = (c.y + 1 until ny).map(Coord(_, c.x)).map(get)
      val dirs = List(left, right, top, bottom)
      dirs.map{ line =>
        val n = get(c)
        val visible = line.takeWhile(_ < n)
        val x = visible.size
        if visible.size == line.size then x else x + 1
      }.product
    }

    def scenic(): List[Int] = (for {
      y <- 0 until ny
      x <- 0 until nx
      c = Coord(y, x)
    } yield score(c)).toList


  val grid = input.map(_.map(_.asDigit).toArray).toArray
  grid.show()

  val res1 = grid.count()
  println(s"res1: $res1")

  val res2 = grid.scenic().max
  println(s"res2: $res2")



