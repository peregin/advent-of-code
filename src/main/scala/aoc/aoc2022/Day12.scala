package aoc.aoc2022

import aoc.Aoc

// S = a
// E = z
object Day12 extends aoc.Aoc("aoc2022/input12.txt", identity):

  type Grid = Array[Array[Char]]

  case class Coord(y: Int, x: Int):
    def +(that: Coord): Coord = Coord(this.y + that.y, this.x + that.x)

  extension (g: Grid)

    def nx: Int = g.head.size
    def ny: Int = g.size
    def show(): Unit = {
      println()
      g.foreach(row => println(row.mkString(" ")))
    }

    def get(c: Coord): Char = g(c.y)(c.x)

    def all(): List[Coord] = (for {
      y <- 0 until ny
      x <- 0 until nx
    } yield Coord(y, x)).toList

    def find(c: Char): Coord = all().find(p => get(p) == c).getOrElse(sys.error("not found"))

    def findAll(c: Char): List[Coord] = all().collect{case p if get(p) == c => p}

    def isInside(c: Coord): Boolean = c.y >= 0 && c.y < ny && c.x < nx && c.x >= 0

    def neighbours(c: Coord): List[Coord] = {
      List(
        Coord(-1, 0), Coord(1, 0),
        Coord(0, -1), Coord(0, 1)
      ).map(_ + c).filter(isInside)
    }

    def validNeighbours(c: Coord, n: List[Coord]): List[Coord] = n.filter(p => get(p).toInt - get(c).toInt <= 1)

  val grid0 = input.map(_.toArray).toArray
  val start = grid0.find('S')
  val end = grid0.find('E')

  val grid = grid0.map(_.map{
    case 'S' => 'a'
    case 'E' => 'z'
    case other => other
  })
  grid0.show()
  println(s"start=$start, finish=$end")

  def bfs(g: Grid, c: Coord, start: Coord, finish: Coord): List[Coord] = {
    val queue = collection.mutable.Queue[Coord]()
    val parents = collection.mutable.HashMap[Coord, Coord]()
    val seen = collection.mutable.HashSet[Coord]()
    queue.enqueue(c)
    var found = false
    while (queue.nonEmpty && !found) {
      val v = queue.dequeue()
      if v == finish then found = true // (deduct from parents)
      else {
        val ns = g.neighbours(v)
        val next = g.validNeighbours(v, ns).filterNot(seen.contains)
        seen ++= next
        next.map(p => parents += p -> v)
        next.foreach(queue.enqueue)
      }
    }
    val res = collection.mutable.ListBuffer[Coord]()
    var i: Option[Coord] = Some(finish)
    while (i.nonEmpty && i != Some(start)) {
      val p = i.flatMap(parents.get)
      p.map(res += _)
      i = p
    }
    res.toList
  }

  val res1 = bfs(grid, start, start, end)
  println(s"res1: ${res1.size}") // 462

  val starters = grid.findAll('a')
  val res2 = starters.map(s => bfs(grid, s, s, end).size).filter(_ > 0).min
  println(s"res2=$res2") // 451




