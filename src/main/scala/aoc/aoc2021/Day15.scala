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
  val nx = grid.head.size
  val ny = grid.size
  val start = (0, 0)
  val end = (ny - 1, nx - 1)

  extension (g: Grid)

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

    @tailrec
    def dijkstra(queue: List[Coord], cost: Map[Coord, Int]): Int =
      val node = queue.head
      if node == end then cost(end)
      else
        val neighbours = g.neighbours(node)
        // dist[u] + length(u, v) < dist[v]
        val candidates = neighbours.filter(c =>
          !cost.contains(c) || cost(c) > cost(node) + g.get(c)
        )
        val newCost = candidates.foldLeft(cost){
          case (cost, c) => cost + (c -> (cost(node) + g.get(c)))
        }
        dijkstra(queue.tail ++ candidates , newCost)

  //grid.show()
  val res1 = grid.dijkstra(List(start), Map((0, 0) -> 0))
  println(s"res1 = $res1")





