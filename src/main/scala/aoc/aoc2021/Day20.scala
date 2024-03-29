package aoc.aoc2021

import aoc.Aoc

import scala.collection.mutable.ArrayBuffer
import scala.util.chaining.*

object Day20 extends Aoc("aoc2021/input20.txt", identity):

  type Grid = Array[Array[Char]]

  extension (g: Grid)

    def show(): Unit = {
      println()
      g.foreach(row => println(row.mkString))
    }

    def expand(k: Int): Grid = {
      val n = g.head.length
      Array.fill(k)(Array.fill(n + 2*k)('.')) ++
        g.map(("."*k).toCharArray ++ _ ++ ("."*k).toCharArray) ++
        Array.fill(k)(Array.fill(n + 2*k)('.'))
    }

    def margin(y: Int, x: Int): Boolean = {
      val ny = g.length
      val nx = g.head.length
      x == 0 || y == 0 || x == nx - 1 || y == ny - 1
    }

    def inside(y: Int, x: Int): Boolean = {
      val ny = g.length
      val nx = g.head.length
      y >= 0 && y < ny && x >= 0 && x < nx
    }

    def decode(y: Int, x: Int, algo: Array[Char]): Char = (for {
      ay <- -1 to 1
      ax <- -1 to 1
      p = if inside(y + ay, x + ax) then g(y + ay)(x + ax) else '.'
    } yield p).mkString
      .pipe(_.replace('.', '0').replace('#', '1'))
      .pipe(bin => Integer.parseInt(bin, 2))
      .pipe(i => algo(i))

    def enhance(algo: Array[Char]): Grid = {
      g.zipWithIndex.map{
        case (row, y) => row.zipWithIndex.map{
          case (c, x) => decode(y, x, algo)
        }
      }
    }


  val (algo, grid) = input.span(_.nonEmpty) match {
    case (first, second) =>
      (first.flatten.toArray, second.map(_.toCharArray).toArray.tail)
  }

  val g2 = grid.expand(1).enhance(algo).expand(1).enhance(algo)
  //g2.show()
  val res1 = g2.map(_.count(_ == '#')).sum
  // 5513 too high
  println(s"res1=$res1")







