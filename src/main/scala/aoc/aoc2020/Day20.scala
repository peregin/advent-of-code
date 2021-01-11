package aoc.aoc2020

import aoc.Aoc

object Day20 extends Aoc("aso2020/input20.txt", identity) {

  case class Tile(id: Long, lines: List[String]) {
    //rotate to left
    def rotate(): Tile = copy(lines = lines.map(_.toList).transpose.map(_.mkString).reverse)

    lazy val top     = lines.head
    lazy val bottom  = lines.last
    lazy val left    = lines.map(_.head).mkString
    lazy val right   = lines.map(_.last).mkString
    lazy val borders = List(right, top, left, bottom)
    lazy val sides   = borders ++ borders.map(_.reverse)

    override def toString: String = s"$id:\n${lines.mkString("\n")}"
  }

  val groups = splitByEmptyLine(input)
  val tiles = groups.map { lines =>
    val id = lines.head.split(' ')(1).stripSuffix(":").toLong
    Tile(id, lines.tail)
  }
  val square = Math.sqrt(tiles.size).toInt
  println(s"tiles=${tiles.size}, square=$square")

  // build a map with the possible border options
  val border2Tile = tiles.foldLeft(Map[String, List[Long]]()) { (accu, tile) =>
    tile.sides.foldLeft(accu) { (accu, side) =>
      val ids = accu.getOrElse(side, Nil)
      accu + (side -> (ids :+ tile.id))
    }
  }

  val corners = tiles.foldLeft(List.empty[Long]) { (accu, tile) =>
    val adjacent = tile.sides.filter(s => border2Tile(s).size > 1)
    //println(s"adjacent[${tile.id}] $adjacent")
    // corners have 4 adjacent values
    if (adjacent.size == 4) accu :+ tile.id else accu
  }

  val solution1 = corners.product
  println(s"solution1=$solution1")
}
