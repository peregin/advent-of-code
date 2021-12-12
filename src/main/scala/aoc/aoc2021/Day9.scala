package aoc.aoc2021

import aoc.Aoc
import aoc.aoc2021.Day11.Grid

import scala.annotation.tailrec

object Day9 extends Aoc("aoc2021/input9.txt", identity):

  type Grid = Array[Array[Int]]
  type Coord = (Int, Int) // y, x

  extension (g: Grid)
    def show(): Unit = {
      println()
      g.foreach(row => println(row.map{
        case d if d > 9 => '.'
        case d => (d + 48).toChar
      }.mkString(" ")))
    }

    def neighbours(c: Coord): Set[Coord] = {
      val (y, x) = c
      List(
        (-1, 0), (1, 0),
        (0, -1), (0, 1)
      ).map{
        case (dy, dx) => (y + dy, x + dx)
      }.filter{
        case (y, x) => y >= 0 && x >= 0 && y < ny && x < nx
      }.toSet
    }

    def get(c: Coord): Int = g(c._1)(c._2)

    def find(): List[Coord] = (for {
      y <- 0 until ny
      x <- 0 until nx
      c = g(y)(x)
      adj = neighbours((y, x))
      if adj.forall(g.get(_) > c)
    } yield (y, x)).toList

    def basinsize(c: Coord): Int = {
      // depth first search the size of the basin
      @tailrec
      def basin(visit: Set[Coord], seen: Set[Coord]): Set[Coord] = {
        val next = visit.flatMap(neighbours(_).filter(get(_) < 9).filterNot(seen.contains))
        if (next.isEmpty) seen
        else basin(next, seen ++ next)
      }
      basin(Set(c), Set.empty).size
    }

  val grid = input.map(_.map(_.asDigit).toArray).toArray
  grid.show()
  val nx = grid.head.size
  val ny = grid.size

  val points = grid.find()
  val res1 = points.map(grid.get(_) + 1).sum
  println(s"res1 = $res1")

  val res2 = points.map(grid.basinsize).sorted.takeRight(3).product
  println(s"res2 = $res2")





