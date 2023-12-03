package aoc.aoc2023

import aoc.Aoc

object Day3 extends aoc.Aoc("aoc2023/input3.txt", identity):

  type Grid = Array[Array[Char]]

  case class Coord(y: Int, x: Int):
    def +(that: Coord): Coord = Coord(this.y + that.y, this.x + that.x)

  extension (g: Grid)
    def show(): Unit = {
      println()
      g.foreach(row => println(row.mkString))
    }
    def nx: Int = g.head.length
    def ny: Int = g.length
    def neighbours(c: Coord): Set[Coord] = (for {
      y <- c._1 - 1 to c._1 + 1 if y >= 0 && y < ny
      x <- c._2 - 1 to c._2 + 1 if x >= 0 && x < nx
    } yield Coord(y, x)).toSet - c

//    def numbers(): List[List[Coord]] = {
//      (y <- 0 until ny).map{ y =>
//
//      }
//    }

  val grid = input.map(_.toArray).toArray
  grid.show()

  val res1 = 1
  println(s"res1: $res1")

//  val res2 = 2
//  println(s"res2: $res2")
