package aoc.aoc2021

import aoc.Aoc

object Day5 extends Aoc("aoc2021/input5.txt", identity):

  case class Position(x: Int, y: Int)

  def generate(a: Int, b: Int) = a to b by (if (a > b) -1 else 1)

  def overlaps(points: List[Position]): Int = {
    val freq = points.groupMapReduce(identity)(_ => 1)(_ + _)
    freq.values.filter(_ > 1).size
  }

  val lines = input.map {
    case s"$x1,$y1 -> $x2,$y2" => (Position(x1.toInt, y1.toInt), Position(x2.toInt, y2.toInt))
  }
  val (straight, diagonal) = lines.partition{ case (a, b) => a.x == b.x || a.y == b.y }

  val pointsStraight = straight.flatMap { case (a, b) =>
    for {
      x <- generate(a.x, b.x)
      y <- generate(a.y, b.y)
    } yield Position(x, y)
  }
  val res1 = overlaps(pointsStraight)
  println(s"res1=$res1")

  val pointsDiagonal = diagonal.flatMap { case (a, b) =>
    for {
      (x, y) <- generate(a.x, b.x).zip(generate(a.y, b.y))
    } yield Position(x, y)
  }
  val res2 = overlaps(pointsDiagonal ++ pointsStraight)
  println(s"res2=$res2")

  //debug()


  def debug(): Unit =
    for {
      y <- 0 to 9
      x <- 0 to 9
    } yield {
      val p = (pointsDiagonal).groupMapReduce(identity)(_ => 1)(_ + _).getOrElse(Position(x, y), 0)
      if (p > 0) print(p)
      else print(".")
      if (x == 9) println()
    }


