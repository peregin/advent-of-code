package aoc.aoc2022

import aoc.Aoc
import aoc.aoc2022.Day22.lastPos

import java.util.StringTokenizer
import scala.jdk.CollectionConverters.*

object Day22 extends aoc.Aoc("aoc2022/input22.txt", identity):

  override protected def shouldTrimInput = false

  enum Move:
    case L, R, F

  case class Coord(y: Int, x: Int):
    def -(that: Coord): Coord = Coord(this.y - that.y, this.x - that.x)
    def +(that: Coord): Coord = Coord(this.y + that.y, this.x + that.x)
    // always positive module
    def %%(that: Coord): Coord = Coord(Math.floorMod(this.y, that.y), Math.floorMod(this.x, that.x))
    def rotateLeft(): Coord    = Coord(-x, y)
    def rotateRight(): Coord   = Coord(x, -y)

  type Board = Array[Array[Char]]
  val right = Coord(0, 1) // starting direction
  val left  = Coord(0, -1)
  val down  = Coord(1, 0)
  val up    = Coord(-1, 0)

  extension (b: Board)
    def maxY() = b.length
    def maxX() = b.map(_.length).max

    def get(c: Coord): Char = {
      val row = b(c.y)
      if row.length <= c.x then ' ' else row(c.x)
    }

    def debug(): Unit =
      for (cy <- 0 until maxY()) {
        for (cx <- 0 until maxX()) {
          val on = Coord(cy, cx)
          val p  = b.get(on)
          val ascii =
            if p == '.' then s"${Console.BLUE}.${Console.RESET}"
            else if p == '#' then s"${Console.BLUE}#${Console.RESET}"
            else if p != ' ' then {
              if debugTurns.contains(on) then s"${Console.RED}$p${Console.RESET}"
              else s"${Console.YELLOW}$p${Console.RESET}"
            } else " "
          print(ascii)
        }
        println()
      }
    end debug

    def next2d(p: Coord, d: Coord): (Coord, Coord) =
      val n = (p + d) %% Coord(maxY(), maxX())
      // println(s"  $p -> $d = $n")
      val check = get(n)
      val step =
        if check == ' ' then
          // detect horizontal and vertical overlap
          if d.y == 0 then {
            if d.x == 1 then Coord(n.y, b(n.y).indexWhere(_ != ' ')) // first from left
            else Coord(n.y, b(n.y).lastIndexWhere(_ != ' '))         // first from right
          } else {
            val col = (0 until maxY()).map(i => b.get(Coord(i, n.x)))
            if d.y == 1 then Coord(col.indexWhere(_ != ' '), n.x) // first from top
            else Coord(col.lastIndexWhere(_ != ' '), n.x)
          }
        else n
      // println(s"[$check] -> [${get(step)}] $p -> $d = $step")
      (step, d)

    /**
     * Cube layout:
     *   1122
     *   33
     * 4455
     * 66
     *
     * Sample is defined as:
     *     11
     * 223344
     *     5566
     *
     * The next3d moves are specific to the input set, not the samples!
     */
    def next3d(p: Coord, d: Coord): (Coord, Coord) = {
      val Coord(y, x) = p
      d match {
        // right
        case Coord(0, 1) if x == 50 - 1 && y >= 150 && y < 200  => (Coord(150 - 1, 50 + y - 150), up)
        case Coord(0, 1) if x == 100 - 1 && y >= 100 && y < 150 => (Coord(50 - 1 - y + 100, 150 - 1), left)
        case Coord(0, 1) if x == 150 - 1 && y >= 0 && y < 50    => (Coord(150 - 1 - y, 100 - 1), left)
        case Coord(0, 1) if x == 100 - 1 && y >= 50 && y < 100  => (Coord(50 - 1, 100 + y - 50), up)
        // left
        case Coord(0, -1) if x == 0 && y >= 150 && y < 200 => (Coord(0, 50 + y - 150), down)
        case Coord(0, -1) if x == 50 && y >= 0 && y < 50   => (Coord(150 - 1 - y, 0), right)
        case Coord(0, -1) if x == 0 && y >= 100 && y < 150 => (Coord(50 - 1 - y + 100, 50), right)
        case Coord(0, -1) if x == 50 && y >= 50 && y < 100 => (Coord(100, y - 50), down)
        // down
        case Coord(1, 0) if y == 50 - 1 && x >= 100 && x < 150 => (Coord(50 + x - 100, 100 - 1), left)
        case Coord(1, 0) if y == 150 - 1 && x >= 50 && x < 100 => (Coord(150 + x - 50, 50 - 1), left)
        case Coord(1, 0) if y == 200 - 1 && x >= 0 && x < 50   => (Coord(0, 100 + x), down)
        // up
        case Coord(-1, 0) if y == 0 && x >= 50 && x < 100  => (Coord(150 + x - 50, 0), right)
        case Coord(-1, 0) if y == 100 && x >= 0 && x < 50  => (Coord(50 + x, 50), right)
        case Coord(-1, 0) if y == 0 && x >= 100 && x < 150 => (Coord(200 - 1, x - 100), up)
        // move
        case _ => (p + d, d)
      }
    }

    def mark(p: Coord, d: Coord): Board =
      b.zipWithIndex.map((line, cy) =>
        if cy == p.y then
          line.zipWithIndex.map((c, cx) =>
            if cx == p.x then
              d match {
                case Coord(0, 1)  => '>'
                case Coord(0, -1) => '<'
                case Coord(1, 0)  => 'v'
                case Coord(-1, 0) => '^'
              }
            else c
          )
        else line
      )

  val plan  = splitByEmptyLine(input)
  val board = plan(0).map(_.toCharArray).toArray
  val instr = new StringTokenizer(plan(1)(0), "LR", true)
    .asIterator()
    .asScala
    .flatMap {
      case "L"        => List(Move.L)
      case "R"        => List(Move.R)
      case fw: String => List.fill(fw.toInt)(Move.F)
    }
    .toList

  val debugTurns = collection.mutable.HashSet[Coord]()
  def find(pos: Coord, dir: Coord, b: Board, moves: List[Move], isCube: Boolean): (Coord, Board) = moves match {
    case Nil => (pos, b)
    case Move.L :: rest =>
      debugTurns += pos
      val nextDir = dir.rotateLeft()
      val bn      = b.mark(pos, nextDir)
      find(pos, nextDir, bn, rest, isCube)
    case Move.R :: rest =>
      debugTurns += pos
      val nextDir = dir.rotateRight()
      val bn      = b.mark(pos, nextDir)
      find(pos, nextDir, bn, rest, isCube)
    case Move.F :: rest =>
      val (next, nextDir) = if isCube then b.next3d(pos, dir) else b.next2d(pos, dir)
      val nextC           = b.get(next)
      if nextC != ' ' && nextC != '#' then
        // update the board and move to the next
        val bn = b.mark(next, nextDir)
        find(next, nextDir, bn, rest, isCube)
      else find(pos, dir, b, rest, isCube) // just stay on this pos, can't move ahead
  }

  // println(s"instructions: $instr")
  val start = Coord(0, board(0).indexOf('.'))
  println(s"starting from $start")

  def password(lastPos: Coord, last: Board): Long = {
    val facing = last.get(lastPos) match {
      case '>' => 0
      case 'v' => 1
      case '<' => 2
      case '^' => 3
    }
    println(s"lastPos=$lastPos, facing=$facing")
    1000 * (lastPos.y + 1) + 4 * (lastPos.x + 1) + facing
  }

  val (lastPos: Coord, last: Board) = find(start, right, board, instr, isCube = false)
  last.debug()
  val res1 = password(lastPos, last)
  println(s"res1: $res1") // 136054 (6032) 207ms

  val (lastPos3d: Coord, last3d: Board) = find(start, right, board, instr, isCube = true)
  val res2                              = password(lastPos3d, last3d)
  println(s"res2: $res2") // 122153
