package aoc

import scala.annotation.tailrec
import scala.io.Source

class Aoc[T](fileName: String, conv: String => T) extends App {

  val input: List[T] = Source
    .fromResource(fileName)
    .getLines()
    .map(_.trim)
    .map(conv)
    .toList

  @tailrec
  final def splitByEmptyLine(lines: List[String], accu:List[List[String]] = Nil): List[List[String]] = {
    if (lines.isEmpty) accu
    else {
      val group = lines.takeWhile(_.nonEmpty)
      splitByEmptyLine(lines.drop(group.size + 1), accu :+ group)
    }
  }
}
