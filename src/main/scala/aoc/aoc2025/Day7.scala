package aoc.aoc2025

object Day7 extends aoc.Aoc("aoc2025/input7.txt", _.toArray):

  type Grid = Array[Array[Char]]
  val grid: Grid = input.toArray

  case class Coord(y: Int, x: Int)

  extension (g: Grid)
    def nx: Int = g.head.length
    def ny: Int = g.length
    def get(c: Coord): Char = g(c.y)(c.x)
    def set(c: Coord, v: Char): Unit = g(c.y)(c.x) = v
    def count(c: Char): Int = g.flatten.count(_ == c)
    def find(c: Char): Coord = (for {
      y <- 0 until ny
      x <- 0 until nx
      if get(Coord(y, x)) == c
    } yield Coord(y, x)).head
    def print(): Unit = g.foreach(r => println(r.mkString))

  val start = grid.find('S')
  val startPos = start
  grid.set(start, '|')
  grid.print()

  var counter = 0
  val res = for {
    y <- 0 until grid.ny - 1
    x <- 0 until grid.nx
    c = Coord(y, x)
    split = if (grid.get(c) == '|') {
      if (grid.get(Coord(y+1, x)) == '^') {
        counter += 1
        grid.set(Coord(y+1, x+1), '|')
        grid.set(Coord(y+1, x-1), '|')
        true
      } else {
        grid.set(Coord(y+1, x), '|')
        false
      }
    } else false
  } yield counter

  println(s"part1: $counter") // 21, 1662

  // need a new grid, ops are mutable on the grid
  val grid2: Grid = input.toArray
  // count all unique timelines by counting paths to endpoints
  def countTimelines(pos: Coord, memo: collection.mutable.Map[Coord, Long] = collection.mutable.Map.empty): Long = {
    memo.getOrElseUpdate(pos, {
      val Coord(y, x) = pos
      if (y >= grid2.ny - 1) 1L
      else {
        val next = Coord(y + 1, x)
        if (grid2.get(next) == '^') {
          countTimelines(Coord(y + 1, x - 1), memo) + countTimelines(Coord(y + 1, x + 1), memo)
        } else countTimelines(next, memo)
      }
    })
  }
  
  val part2 = countTimelines(startPos)
  println(s"part2: $part2") // 40, 40941112789504

