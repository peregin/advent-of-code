package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day4 extends Aoc("aoc2021/input4.txt", identity):

  case class Tile(number: Int, var marked: Boolean)
  case class Board(lines: List[String]):
    val grid: Array[Array[Tile]] = lines.map{ line =>
      line.split(' ').filter(_.trim.nonEmpty).map(n => Tile(n.toInt, false))
    }.toArray
    def mark(n: Int): Unit = {
      for (i <- 0 until grid.length; j <- 0 until grid(i).length) {
        if (grid(i)(j).number == n) grid(i)(j).marked = true
      }
    }
    def solved(): Boolean = grid.exists(_.forall(_.marked)) || grid.transpose.exists(_.forall(_.marked))
    def sum(): Int = grid.flatten.filterNot(_.marked).map(_.number).sum

  // first group is the drawn numbers
  // the rest of groups represents a board
  val groups = splitByEmptyLine(input)
  val drawn = groups.head.head.split(',').map(_.toInt).toList
  val boards = groups.tail.map(Board)

  def find(in: List[Int]): (Int, Board) = {
    val n = in.head
    boards.foreach(_.mark(n))
    boards.find(_.solved()) match {
      case Some(bingo) => (n, bingo)
      case None => find(in.tail)
    }
  }

  val (n, bingo) = find(drawn)
  val res = n * bingo.sum()
  println(s"res=$res")
