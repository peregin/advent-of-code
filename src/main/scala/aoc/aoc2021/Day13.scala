package aoc.aoc2021

import aoc.Aoc

object Day13 extends Aoc("aoc2021/input13.txt", identity):

  type Row = Array[Char]
  type Grid = Array[Row]
  type Coord = (Int, Int) // y, x

  val (dots, instructions) = input.filter(_.trim.nonEmpty).partition(_.charAt(0).isDigit)
  val coords: Set[Coord] = dots.map{ c =>
    val a = c.split(',')
    (a(0).toInt, a(1).toInt) // x,y format
  }.toSet
  val nx = coords.map(_._1).max + 1
  val ny = coords.map(_._2).max + 1

  println(s"nx: $nx, ny: $ny, instructions: ${instructions.length}, coords: ${coords.size}")

  val grid = Array.fill(ny, nx)('.').zipWithIndex.map{
    case (row, y) => row.zipWithIndex.map{
      case (d, x) => if (coords.contains((x, y))) '#' else d
    }
  }

  // alternatively use a set of coordinates where mirroring x or y
  extension (g: Grid)
    def show(): Unit =
      println()
      g.foreach(row => println(row.mkString))

    def merge(a: Row, b: Row): Row = a.zip(b).map{
      case (a, b) => if a == '#' || b == '#' then '#' else '.'
    }

    def foldY(at: Int): Grid =
      val (splitAt, fill) = if g.length % 2 > 0 then (at, Array.fill(g.head.length)('.')) else (at, Array.empty[Char])
      val (top, bottom) = g.splitAt(splitAt)
      (top).zip(Array(fill) ++ bottom.reverse).map{
        case (row1, row2) => merge(row1, row2)
      }

    def foldX(at: Int): Grid = g.map{ row =>
      val (splitAt, fill) = if g.head.length % 2 > 0 then (at, Array.empty[Char]) else (at + 1, Array('.'))
      val (left, right) = row.splitAt(splitAt)
      merge(left, fill ++ right.reverse)
    }

  def step(g: Grid, instr: List[String]): Grid = instr match {
    case Nil => g
    case head :: tail =>
      //println(s"step: $head")
      head match {
        case s"fold along y=$at" => step(g.foldY(at.toInt), tail)
        case s"fold along x=$at" => step(g.foldX(at.toInt), tail)
      }
  }
  //grid.foldY(7).show()
  //grid.foldY(7).foldX(5).show()

  // 687
  val res1 = step(grid, instructions.take(1))
  println(s"res1=${res1.map(_.count(_ == '#')).sum}")

  val res2 = step(grid, instructions)
  res2.show()
  //println(s"res22=${res2.map(_.count(_ == '#')).sum}, y=${res2.size}")