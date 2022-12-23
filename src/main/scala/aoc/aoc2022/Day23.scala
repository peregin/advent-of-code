package aoc.aoc2022

import aoc.Aoc
import aoc.aoc2022.Day23.Direction
import aoc.aoc2022.Day23.Direction._

object Day23 extends aoc.Aoc("aoc2022/input23.txt", identity):

  case class Coord(y: Int, x: Int):
    def +(that: Coord): Coord = Coord(this.y + that.y, this.x + that.x)

    lazy val neighbours: Set[Coord] = allDirs.map(_ + this)

  val elves = input.zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex.flatMap {
        case (c, x) if c == '#' => Some(Coord(y, x))
        case _                  => None
      }
    )
    .toSet

  enum Direction(val dir: Coord):
    case North     extends Direction(Coord(-1, 0))
    case East      extends Direction(Coord(0, 1))
    case South     extends Direction(Coord(1, 0))
    case West      extends Direction(Coord(0, -1))
    case NorthEast extends Direction(Coord(-1, 1))
    case SouthEast extends Direction(Coord(1, 1))
    case SouthWest extends Direction(Coord(1, -1))
    case NorthWest extends Direction(Coord(-1, -1))

  val allDirs = Direction.values.map(_.dir).toSet

  // sequence of checking matters, in each round the order is shifted
  type DirectionCheck = (Direction, Set[Coord])
  val moves = List[DirectionCheck](
    North -> Set(North, NorthEast, NorthWest).map(_.dir),
    South -> Set(South, SouthEast, SouthWest).map(_.dir),
    West  -> Set(West, NorthWest, SouthWest).map(_.dir),
    East  -> Set(East, NorthEast, SouthEast).map(_.dir)
  )

  case class State(elves: Set[Coord], moves: List[DirectionCheck], stale: Boolean):
    def nextMoves() = moves.tail :+ moves.head
    // topLeft, bottomRight
    def area(): (Coord, Coord) = elves.foldLeft((Coord(Int.MaxValue, Int.MaxValue), Coord(Int.MinValue, Int.MinValue)))((accu, p) =>
      var (a, b) = accu
      if p.y < a.y then a = a.copy(y = p.y)
      if p.x < a.x then a = a.copy(x = p.x)
      if p.y > b.y then b = b.copy(y = p.y)
      if p.x > b.x then b = b.copy(x = p.x)
      (a, b)
    )

    def points(): Int =
      val (topLeft, bottomRight) = area()
      val h = bottomRight.y - topLeft.y + 1
      val w = bottomRight.x - topLeft.x + 1
      h * w - elves.size

  def step(state: State): State =
    val elves = state.elves
    // plan the next moves
    val stay = elves.filter(_.neighbours.forall(p => !elves.contains(p)))
    val plan = (elves -- stay).map { elf =>
      val maybeMove = state.moves.find { case (_, check) => check.map(_ + elf).forall(p => !elves.contains(p)) }.map(_._1.dir)
      elf -> maybeMove.map(_ + elf)
    }
    val cantMove = plan.filter(_._2.isEmpty).map(_._1)
    // detect elves targeting the same move
    val target2Elf             = plan.collect { case (e, Some(m)) => e -> m }.groupMap(_._2)(_._1)
    val (canMove, crowdedMove) = target2Elf.partition(_._2.size == 1)
    val newPos                 = canMove.keys.toSet
    val oldPos = canMove.values.flatten.toSet
    State(
      elves = crowdedMove.values.flatten.toSet ++ cantMove ++ stay ++ newPos -- oldPos,
      moves = state.nextMoves(),
      stale = newPos.isEmpty
    )
  end step

  def debug(state: State): Unit =
    val (topLeft, bottomRight) = state.area()
    for (cy <- topLeft.y to bottomRight.y) {
      for (cx <- topLeft.x to bottomRight.x) {
        val c = Coord(cy, cx)
        print(if state.elves.contains(c) then "#" else s"${Console.BLUE}.${Console.RESET}")
      }
      println()
    }
  end debug

  val init = State(elves, moves, false)
  //debug(init)
  //println()
  val last = LazyList.iterate(init)(step).take(11).last
  debug(last)
  val res1 = last.points()
  println(s"res1: $res1") // 4195 (110)

  val res2 = LazyList.iterate(init)(step).zipWithIndex.find(_._1.stale == true).get._2
  println(s"res2: $res2") // 1069 (20)
