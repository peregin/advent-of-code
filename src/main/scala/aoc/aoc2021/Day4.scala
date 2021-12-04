package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day4 extends Aoc("aoc2021/input4.txt", identity):

  // first group is the drawn numbers
  // the rest of groups represents a board
  val groups = splitByEmptyLine(input)
  val drawn = groups.head

  case class Tile(number: Int, marked: Boolean)

  case class Board(lines: List[String]):
    val grid: Array[Array[Tile]] = lines.map(_.split(' ').map(n => Tile(n.toInt, false))).toArray

  println("res")
