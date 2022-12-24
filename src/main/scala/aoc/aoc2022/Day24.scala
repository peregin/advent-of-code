package aoc.aoc2022

import aoc.Aoc
import aoc.aoc2022.Day22.Coord

object Day24 extends aoc.Aoc("aoc2022/input24.txt", identity):

  val size = Coord(input.length, input.head.length)
  println(s"size=$size")

  case class Coord(y: Int, x: Int):
    def +(that: Coord): Coord = Coord(this.y + that.y, this.x + that.x)

  val start = Coord(0, 1)
  val end   = Coord(size.y - 1, size.x - 2)
  case class Blizzard(pos: Coord, dir: Direction):
    def next() = Blizzard(
      pos = this.pos + dir.dir match {
        case Coord(y, 0)                      => Coord(y, size.x - 2)
        case Coord(y, cx) if cx == size.x - 1 => Coord(y, 1)
        case Coord(0, x)                      => Coord(size.y - 2, x)
        case Coord(cy, x) if cy == size.y - 1 => Coord(1, x)
        case other                            => other
      },
      dir = this.dir
    )

  enum Direction(val dir: Coord, val look: Char):
    case North extends Direction(Coord(-1, 0), '^')
    case East  extends Direction(Coord(0, 1), '>')
    case South extends Direction(Coord(1, 0), 'v')
    case West  extends Direction(Coord(0, -1), '<')
  val look2Direction = Direction.values.groupMapReduce(_.look)(identity)((a, _) => a)
  val neighbours     = look2Direction.values.map(_.dir).toSet

  // parse input
  val (wall, blizzard) = input.zipWithIndex
    .flatMap((line, y) => line.zipWithIndex.map((c, x) => (c, Coord(y, x))))
    .foldLeft((Set.empty[Coord], Set.empty[Blizzard])) {
      case ((accuWall, accuBlizzard), ('#', pos))           => (accuWall + pos, accuBlizzard)
      case ((accuWall, accuBlizzard), (c, pos)) if c != '.' => (accuWall, accuBlizzard + Blizzard(pos, look2Direction(c)))
      case ((accuWall, accuBlizzard), _)                    => (accuWall, accuBlizzard)
    }

  def debug(bs: Set[Blizzard], me: Coord, time: Int): Unit =
    val blizzard2Looks = bs.groupMap(_.pos)(_.dir.look)
    for (cy <- 0 until size.y) {
      for (cx <- 0 until size.x) {
        val c = Coord(cy, cx)
        print(
          if me == c then s"${Console.RED}@${Console.RESET}"
          else if end == c then s"${Console.RED}X${Console.RESET}"
          else if wall.contains(c) then s"${Console.BLUE}#${Console.RESET}"
          else if blizzard2Looks.contains(c) then
            val look = blizzard2Looks(c) match {
              case looks if looks.size == 1 => looks.head
              case looks                    => looks.size.toString.head
            }
            s"${Console.YELLOW}$look${Console.RESET}"
          else s"${Console.BLUE}.${Console.RESET}"
        )
      }
      println()
    }
    println(s"time: $time")
  end debug

  // test how the blizzard is moving
  // LazyList.iterate(blizzard)(_.map(_.next())).zipWithIndex.take(19).tapEach((b, ix) => debug(b, start, ix)).last
  // debug(blizzard, start)

  @annotation.tailrec
  def explore(b: Set[Blizzard], options: Set[Coord], target: Coord, time: Int): (Int, Set[Blizzard]) =
    val nextb  = b.map(_.next())
    val nextbc = nextb.map(_.pos)
    // do not allow to move outside the walls and should not cover walls and blizzards
    val nextOptions = options
      .flatMap(p => neighbours.map(_ + p) + p)
      .filter(p => p.y >= 0 && p.y <= end.y)
      .filterNot(nextbc.contains)
      .filterNot(wall.contains)
    if nextOptions.contains(target) then (time, nextb)
    else explore(nextb, nextOptions, target, time + 1)

  val (time1, b1) = explore(blizzard, Set(start), end, 1)
  val res1        = time1
  println(s"res1: $res1") // 290 (18)

  val (time2, b2) = explore(b1, Set(end), start, 1)
  val (time3, b3)  = explore(b2, Set(start), end, 1)
  val res2        = time1 + time2 + time3
  println(s"res2: $res2") // 842 (54)

  debug(b3, end, res2)