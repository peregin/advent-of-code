package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day15 extends Aoc("aoc2021/input15.txt", _.map(_.asDigit).toArray):

  type Grid = Array[Array[Int]]
  type Coord = (Int, Int) // y, x

  val deltas = List(
    (-1, 0), (1, 0),
    (0, -1), (0, 1)
  )

  val grid = input.toArray

  extension (g: Grid)

    def nx: Int = g.head.size
    def ny: Int = g.size
    def start: Coord = (0, 0)
    def end: Coord = (ny - 1, nx - 1)

    def show(): Unit = {
      println()
      g.foreach(row => println(row.mkString))
    }

    def neighbours(c: Coord): Set[Coord] = {
      val (y, x) = c
      deltas.map{
        case (dy, dx) => (y + dy, x + dx)
      }.filter{
        case (y, x) => y >= 0 && x >= 0 && y < ny && x < nx
      }.toSet
    }

    def get(c: Coord): Int = g(c._1)(c._2)

    def find(): Int = dijkstra(Set(start), Map((0, 0) -> 0))

    @tailrec
    def dijkstra(queue: Set[Coord], cost: Map[Coord, Int]): Int =
      val node = queue.minBy(cost)
      if node == end then cost(end)
      else
        val neighbours = g.neighbours(node)
        // dist[u] + length(u, v) < dist[v]
        val candidates = neighbours.filter(c =>
          !cost.contains(c) || cost(c) > cost(node) + get(c)
        )
        val newCosts = candidates.foldLeft(cost){
          case (cost, c) => cost + (c -> (cost(node) + get(c)))
        }
        dijkstra(queue - node ++ candidates, newCosts)

  //grid.show()
  val res1 = grid.find()
  println(s"res1 = $res1")

  val n = 5
  val h = grid.ny
  val w = grid.nx
  val grid2 = (0 until grid.ny * n).map{y=>
    (0 until grid.nx * n).map{x=>
      val p = grid(y % h)(x % w)
      val (tx, ty) = (y / h, x / w)
      1 + (p - 1 + ty + tx) % 9
    }.toArray
  }.toArray
  //grid2.show()
  val res2 = grid2.find()
  println(s"res2 = $res2")




