package aoc.aoc2024

import aoc.Aoc

import scala.annotation.targetName

object Day8 extends Aoc("aoc2024/input8.txt", _.toArray):

  type Grid = Array[Array[Char]]

  val grid: Grid = input.toArray

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
    def set(c: Coord, v: Char): Unit = g(c.y)(c.x) = v
    def isInside(c: Coord): Boolean  = c.y >= 0 && c.y < ny && c.x < nx && c.x >= 0
    def show(): Unit = {
      println()
      g.foreach(row =>
        println(row.map {
          case d if d == '.' => Console.YELLOW + d + Console.RESET
          case o             => o
        }.mkString(""))
      )
    }

  grid.show()

  val all = for {
    y <- 0 until grid.ny
    x <- 0 until grid.nx
    c = Coord(y, x)
    v = grid.get(c) if v != '.' && grid.isInside(c)
  } yield (v, c)
  val antennas = all.groupMap(_._1)(_._2)
  println(antennas)

  def antinodes(c1: Coord, c2: Coord): Set[Coord] = Set(
    c1 + (c1 - c2),
    c2 + (c2 - c1)
  ).filter(grid.isInside)
  def antinodes2(c1: Coord, c2: Coord): Set[Coord] = {
    val step1 = c1 - c2
    val step2 = c2 - c1
    LazyList.iterate(c1)(s => s + step1).takeWhile(grid.isInside).toSet ++ LazyList.iterate(c2)(s => s + step2).takeWhile(grid.isInside)
  }

  def calculate(func: (Coord, Coord) => Set[Coord]): Int = {
    val antis = antennas.values.toSet.flatMap(same => same.combinations(2).flatMap(pair => func(pair.head, pair.last)))
    // antis.foreach(grid.set(_, '#'))
    // grid.show()
    antis.size
  }
  println(s"part1=${calculate(antinodes)}")
  println(s"part2=${calculate(antinodes2)}")
