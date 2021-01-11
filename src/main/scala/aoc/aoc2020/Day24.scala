package aoc.aoc2020

import aoc.Aoc

import scala.annotation.tailrec

/**
  * These directions are given in your list, respectively, as e, se, sw, w, nw, and ne.
  * Use cube coordinates, see https://www.redblobgames.com/grids/hexagons/
  */
object Day24 extends Aoc("aso2020/input24.txt", identity) {

  case class Tile(x: Int, y: Int, z: Int) {
    def e()  = Tile(x + 1, y - 1, z)
    def se() = Tile(x, y - 1, z + 1)
    def sw() = Tile(x - 1, y, z + 1)
    def w()  = Tile(x - 1, y + 1, z)
    def nw() = Tile(x, y + 1, z - 1)
    def ne() = Tile(x + 1, y, z - 1)

    lazy val neighbours = Set(e(), se(), sw(), w(), nw(), ne())
  }
  val reference = Tile(0, 0, 0)

  @tailrec
  def walk(curr: Tile, coord: String): Tile =
    if (coord.isEmpty) curr
    else {
      val (next, rest) = coord match {
        case s"e$rest"  => (curr.e(), rest)
        case s"se$rest" => (curr.se(), rest)
        case s"sw$rest" => (curr.sw(), rest)
        case s"w$rest"  => (curr.w(), rest)
        case s"nw$rest" => (curr.nw(), rest)
        case s"ne$rest" => (curr.ne(), rest)
      }
      walk(next, rest)
    }

  //println(walk(reference, "nwwswee")) // goes back to the origins

  val tilesReached = input.map(walk(reference, _)).groupBy(identity).view.mapValues(_.size)
  val blacks       = tilesReached.toList.filter(_._2 % 2 > 0).map(_._1).toSet

  val solution1 = blacks.size
  println(s"solution1=$solution1")

  // Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
  // Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
  // Keep counting how many tiles stay black and how many tiles turn black
  def flip(blacks: Set[Tile]): Set[Tile] =
    blacks.flatMap { tile =>
      // check how many stay as black tile
      val adjacentToBlack       = tile.neighbours
      val adjacentBlacksToBlack = blacks intersect adjacentToBlack
      val stayBlacks            = if (adjacentBlacksToBlack.size == 0 || adjacentBlacksToBlack.size > 2) List.empty else List(tile)

      // check how many turn to black tile
      val turnBlacks = adjacentToBlack.filter(!blacks.contains(_)).filter { white =>
        val adjacentToWhite       = white.neighbours
        val adjacentBlacksToWhite = blacks intersect adjacentToWhite
        adjacentBlacksToWhite.size == 2
      }
      (stayBlacks ++ turnBlacks).toSet
    }

  val solution2 = (1 to 100).foldLeft(blacks) { (accu, ix) =>
    val res = flip(accu)
    println(s"$ix = ${res.size}")
    res
  }
  println(s"solution2=${solution2.size}")
}
