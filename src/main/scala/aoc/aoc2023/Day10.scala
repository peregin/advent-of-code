package aoc.aoc2023

import aoc.Aoc

object Day10 extends aoc.Aoc("aoc2023/input10_1.txt", identity):

  type Grid = Array[Array[Char]]

  val down     = Coord(-1, 0)
  val up       = Coord(1, 0)
  val right    = Coord(0, 1)
  val left     = Coord(0, -1)
  val adjacent = Set(down, up, right, left)

  case class Coord(y: Int, x: Int):
    def +(that: Coord): Coord = Coord(this.y + that.y, this.x + that.x)

  extension (g: Grid)
    def nx: Int                           = g.head.length
    def ny: Int                           = g.length
    def neighbours(c: Coord): Set[Coord] = adjacent.map(c + _)
    def isInside(c: Coord): Boolean       = c.y >= 0 && c.y < ny && c.x < nx && c.x >= 0
    def at(c: Coord): Char                = g(c.y)(c.x)
    def filter(c: Char): List[Coord] = (for {
      y <- 0 until ny
      x <- 0 until nx
      if grid(y)(x) == c
    } yield Coord(y, x)).toList

    def explore(c: Coord): Set[Coord] = at(c) match {
      case '-' => Set(c + left, c + right)
      case '|' => Set(c + up, c + down)
      case 'F' => Set(c + right, c + down)
      case '7' => Set(c + left, c + down)
      case 'J' => Set(c + left, c + up)
      case 'L' => Set(c + right, c + up)
      case 'S' => adjacent.map(c + _).filter(p => at(p) != '.')
    }

    def bfs(start: Coord): List[Coord] = {
      val queue   = collection.mutable.Queue[Coord]()
      val parents = collection.mutable.HashMap[Coord, Coord]()
      val seen    = collection.mutable.HashSet[Coord]()
      queue.enqueue(start)
      var found = false
      while (queue.nonEmpty && !found) {
        val v = queue.dequeue()
        if (neighbours(v).contains(start)) found = true
        else {
          val ns = explore(v).filter(isInside).filter(_ != v)
          val next = ns.filterNot(seen.contains)
          seen ++= next
          next.map(p => parents += p -> v)
          next.foreach(queue.enqueue)
        }
      }
      val res              = collection.mutable.ListBuffer[Coord]()
      var i: Option[Coord] = Some(start)
      while (i.nonEmpty && !i.contains(start)) {
        val p = i.flatMap(parents.get)
        p.map(res += _)
        i = p
      }
      res.toList
    }
    def show(): Unit = {
      println()
      for {
        y <- 0 until grid.ny;
        x <- 0 until grid.nx
      } {
        val p = Coord(y, x)
        val text = grid.at(p) match {
          case '.' => s"${Console.BLUE}.${Console.RESET}"
          case 'S' => s"${Console.GREEN}*${Console.RESET}"
          case '-' => s"${Console.YELLOW}-${Console.RESET}"
          case '|' => s"${Console.YELLOW}|${Console.RESET}"
          case 'F' => s"${Console.YELLOW}⌌${Console.RESET}"
          case '7' => s"${Console.YELLOW}⌍${Console.RESET}"
          case 'J' => s"${Console.YELLOW}⌏${Console.RESET}"
          case 'L' => s"${Console.YELLOW}⌞${Console.RESET}"
        }
        print(text)
        if (x == grid.nx - 1) println()
      }
    }

  val grid = input.map(_.toArray).toArray
  grid.show()

  val start = grid.filter('S').head
  println(s"start: $start")
  val path = grid.bfs(start)
  println(s"cycle: $path")

  val res1 = 1
  println(s"res1: $res1") //

  val res2 = 2
  println(s"res2: $res2") //
