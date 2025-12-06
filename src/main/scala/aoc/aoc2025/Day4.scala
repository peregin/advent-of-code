package aoc.aoc2025

object Day4 extends aoc.Aoc("aoc2025/input4.txt", _.toArray):

  type Grid = Array[Array[Char]]
  val grid: Grid = input.toArray
  case class Coord(y: Int, x: Int)

  extension (g: Grid)
    def nx: Int = g.head.length
    def ny: Int = g.length
    def get(c: Coord): Char = g(c.y)(c.x)
    def set(c: Coord, v: Char): Unit = g(c.y)(c.x) = v
    def neighbours(c: Coord): Set[Coord] = (for {
      y <- c.y - 1 to c.y + 1 if y >= 0 && y < ny
      x <- c.x - 1 to c.x + 1 if x >= 0 && x < nx
    } yield Coord(y, x)).toSet - c
    def accessible(): Set[Coord] = 
      (for {
        y <- 0 until ny
        x <- 0 until nx
        c = Coord(y, x)
        if get(c) == '@' && neighbours(c).count(get(_) == '@') < 4
      } yield c).toSet
    def count(c: Char): Int = g.flatten.count(_ == c)
    def print(): Unit = g.foreach(r => println(r.mkString))

  val part1 = grid.accessible().size
  println(s"part1=$part1") // 1363

  @annotation.tailrec
  def grab(g: Grid, counter: Int = 0): Int = {
    val clean = g.accessible()
    if (clean.isEmpty) counter else {
      clean.foreach(c => g.set(c, 'x'))
      grab(g, counter + 1)
    }
  }

  grab(grid)
  val part2 = grid.count('x')
  //grid.print()
  println(s"part2=$part2") // 43, 8184

