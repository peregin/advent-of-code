package aoc.aoc2022

import aoc.Aoc

object Day14 extends aoc.Aoc("aoc2022/input14.txt", identity):

  object Coord:
    def from(s: String): Coord = {
      val arr = s.split(',')
      new Coord(arr(0).toInt, arr(1).toInt)
    }
  case class Coord(x: Int, y: Int):

    def -(that: Coord): Coord = Coord(this.x - that.x, this.y - that.y)
    def +(that: Coord): Coord = Coord(this.x + that.x, this.y + that.y)
    def between(that: Coord): List[Coord] = that - this match {
      case Coord(0, dy) => (that.y to this.y by -dy.sign).map(Coord(this.x, _)).toList
      case Coord(dx, 0) => (that.x to this.x by -dx.sign).map(Coord(_, this.y)).toList
    }

  def expand(c: Coord, rest: List[Coord]): List[Coord] = rest match {
    case Nil => Nil
    case h :: next => c.between(h) ++ expand(h, next)
  }

  val rocks: Set[Coord] = input
    .map(
      line => line
        .split("->")
        .map(_.trim)
        .map(Coord.from)
        .toList
    ).flatMap(coords => expand(coords.head, coords.tail)).toSet

  def debug(rock: Set[Coord], sand: Set[Coord], x0: Int, x1: Int, y0: Int, y1: Int): Unit = {
    for (cy <- y0 to y1) {
      for (cx <- x0 to x1) {
        val c = Coord(cx, cy)
        print(if rock.contains(c) then s"${Console.RED}#${Console.RESET}"
          else if sand.contains(c) then s"${Console.YELLOW}o${Console.RESET}"
          else s"${Console.BLUE}.${Console.RESET}")
      }
      println()
    }
  }

  val start = Coord(500, 0)
  val moves = List(Coord(0, 1), Coord(-1, 1), Coord(1, 1))

  def path(c: Coord, sand: Set[Coord], rock: Set[Coord], lastY: Int): List[Coord] = {
    val maybeNext = moves.map(_ + c).filterNot(p => rock.contains(p) || sand.contains(p)).headOption
    maybeNext match {
      case None => List(c)
      case Some(p) if p.y >= lastY + 1 => List(c)
      case Some(p) => c +: path(p, sand, rock, lastY)
    }
  }

  @annotation.tailrec
  def flow(sand: Set[Coord], rock: Set[Coord], lastY: Int): Set[Coord] = {
    if sand.contains(start) then sand else {
      val last = path(start, sand, rock, lastY).last
      if last.y >= lastY then sand
      else flow(sand + last, rock, lastY)
    }
  }

  val endY = rocks.map(_.y).max
  val minX = rocks.map(_.x).min-1
  val maxX = rocks.map(_.x).max+1
  val sand = flow(Set.empty[Coord], rocks, endY)

  debug(rocks, sand, minX, maxX, start.y, endY+1)
  val res1 = sand.size
  println(s"res1: $res1") // 828

  val diff = (maxX - minX) * 3
  val bottom = ((start.x - diff) to (start.x + diff)).map(Coord(_, endY + 2)).toSet
  val newRocks = rocks ++ bottom

  val sand2 = flow(Set.empty[Coord], newRocks, endY+10)
  //debug(newRocks, sand2, newRocks.map(_.x).min-1, newRocks.map(_.x).max+1, start.y, endY+3)
  val res2 = sand2.size
  println(s"res2: $res2") // 25500







