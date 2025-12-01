package aoc.aoc2025

// Dial from 0 to 99, it starts from 50, can turn left and right
object Day1 extends aoc.Aoc("aoc2025/input1.txt", identity):

  private sealed abstract class Turn(click: Int)
  private case class Left(click: Int) extends Turn(click)
  private case class Right(click: Int) extends Turn(click)
  
  private val codes = input.map {
    case s"L$c" => Left(c.toInt)
    case s"R$c" => Right(c.toInt)
  }

  private def point(from: Int, turn: Turn): Int = turn match {
    case Right(click) => (from + click) % 100
    case Left(click) => (from - click + 100) % 100
  }

  // convert the list to dial points
  private def dial(from: Int, turns: List[Turn]): List[Int] = turns.foldLeft(List(from)) { (points, turn) =>
    points :+ point(points.last, turn)
  }

  private def countZeros(from: Int, turn: Turn): Int = turn match {
    case Right(click) => (1 to click).count(i => (from + i) % 100 == 0)
    case Left(click) => (1 to click).count(i => (from - i + 100) % 100 == 0)
  }

  val (part1, part2) = codes.foldLeft((50, 0, 0)) { case ((pos, finalZeros, pathZeros), turn) =>
    val newPos = point(pos, turn)
    val zerosInPath = countZeros(pos, turn)
    (newPos, if (newPos == 0) finalZeros + 1 else finalZeros, pathZeros + zerosInPath)
  } match { case (_, finalZeros, pathZeros) => (finalZeros, pathZeros) }
  
  println(s"part1: $part1") // 1154
  println(s"part2: $part2") // 6819
