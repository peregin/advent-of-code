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
      y <- c.y - 1 to c.y + 1 if y >= 0 && y < ny
      x <- c.x - 1 to c.x + 1 if x >= 0 && x < nx
    } yield Coord(y, x)).toSet - c

    def at(c: Coord): Char = g(c.y)(c.x)
    def isDigit(c: Coord): Boolean = at(c).isDigit

    def isInside(c: Coord): Boolean = c.y >= 0 && c.y < ny && c.x < nx && c.x >= 0

    def numbers(): List[List[Coord]] =
      (0 until ny).foldLeft(List[List[Coord]]())((accy, y) =>
        (0 until nx).foldLeft(accy)((accx, x) =>
          val c = Coord(y, x)
          if (isDigit(c)) {
            val p = Coord(y, x - 1)
            if (isInside(p) && isDigit(p)) {
              accx.lastOption match {
                case Some(last) => accx.dropRight(1) :+ (last :+ c) // append as next digit to the last group
                case None => List(List(c)) // new in the list
              }
            } else accx :+ List(c) // prev was not digit or first element, add as new
          } else accx
        )
      )

    def partNumber(p: List[Coord]): Boolean = p.exists(n => neighbours(n).exists(c => !g.isDigit(c) && at(c) != '.'))

    def partString(p: List[Coord]): String = p.map(c => at(c)).mkString

    def filter(c: Char): List[Coord] = (for {
      y <- 0 until ny
      x <- 0 until nx
      if grid(y)(x) == c
    } yield Coord(y, x)).toList

  val grid = input.map(_.toArray).toArray
  //grid.show()

  val numCoords = grid.numbers()
  //println(numCoords.map(grid.partString))
  val partCoords = numCoords.filter(grid.partNumber)
  val res1 = partCoords.map(grid.partString).map(_.toInt).sum

  println(s"res1: $res1") // 539713

  val stars = grid.filter('*')
  val gearCoords = stars.map(grid.neighbours).map(ns => partCoords.filter(_.exists(ns.contains))).filter(_.size == 2)
  val res2 = gearCoords.map(pairs => pairs.map(grid.partString).map(_.toLong)).map(_.product).sum

  // debugging
  val gears = gearCoords.flatten.flatten
  for {y <- 0 until grid.ny;
       x <- 0 until grid.nx} {
    val p = Coord(y, x)
    val c = grid.at(p)
    val text =
      if (c == '.') s"${Console.BLUE}.${Console.RESET}"
      else if (c == '*') s"${Console.RED}*${Console.RESET}"
      else if (gears.contains(p)) s"${Console.YELLOW}$c${Console.RESET}"
      else c.toString
    print(text)
    if (x == grid.nx - 1) println()
  }

  println(s"res2: $res2") // 84159075
