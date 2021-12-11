package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day11 extends Aoc("aoc2021/input11.txt", identity):

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

    def plus(): Grid = g.map(row => row.map(_ + 1))

    def neighbours(c: Coord): Set[Coord] = (for{
      y <- c._1 - 1 to c._1 + 1 if y >= 0 && y < ny
      x <- c._2 - 1 to c._2 + 1 if x >= 0 && x < nx
    } yield (y, x)).toSet

    def flashes(): Set[Coord] = (for {
      y <- 0 until ny
      x <- 0 until nx
      if g(y)(x) > 9
    } yield (y, x)).toSet

    def plus(on: Set[Coord]): Grid = g.zipWithIndex.map{
      case (row, y) => row.zipWithIndex.map{
        case (d, x) => if (on.contains((y, x))) d + 1 else d
      }
    }

    def resetFlashes() = g.map(row => row.map {
      case c if c > 9 => 0
      case c => c
    })

    def countFlashes(): Int = g.map(row => row.count(_ == 0)).sum

    def isCleared(): Boolean = countFlashes() == nx * ny

  val grid = input.map(_.map(_.asDigit).toArray).toArray
  grid.show()
  val nx = grid.head.size
  val ny = grid.size

  def step(in: Grid): Grid = {
    @tailrec
    def flash(g: Grid, flashes: Set[Coord] = Set.empty): Grid = {
      val newFlashes = g.flashes() -- flashes
      if (newFlashes.isEmpty) g
      else {
        val ng = newFlashes.foldLeft(g){
          case (accu, p) => accu.plus(g.neighbours(p))
        }
        flash(ng, newFlashes ++ flashes)
      }
    }
    flash(in.plus()).resetFlashes()
  }

  def count(in: Grid, n: Int): Int = {
    if n == 0 then 0
    else in.countFlashes() + count(step(in), n - 1)
  }
  val res1 = count(grid, 101)
  println(s"res = $res1")

  @tailrec
  def find(in: Grid, n: Int): Int = {
    if in.isCleared() then n
    else find(step(in), n + 1)
  }

  val res2 = find(grid, 0)
  println(s"res = $res2")



