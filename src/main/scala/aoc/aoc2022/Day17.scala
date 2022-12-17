package aoc.aoc2022

import aoc.Aoc

object Day17 extends aoc.Aoc("aoc2022/input17.txt", identity):

  type JetStream = LazyList[Char]
  type ShapeStream = LazyList[Shape]
  type Chamber = Set[Coord]

  case class Coord(x: Int, y: Int):
    def -(that: Coord): Coord = Coord(this.x - that.x, this.y - that.y)
    def +(that: Coord): Coord = Coord(this.x + that.x, this.y + that.y)
    def dy(d: Int): Coord = this.copy(y = this.y + d)
  end Coord

  val startMove = Coord(2, 3)
  val leftMove = Coord(-1, 0)
  val rightMove = Coord(1, 0)
  val downMove = Coord(0, -1)

  case class Shape(name: String, points: Set[Coord]):
    def move(c: Coord): Shape = this.copy(points = this.points.map(_ + c))
    def minY(): Int = points.map(_.y).min
    def minX(): Int = points.map(_.x).min
    def maxX(): Int = points.map(_.x).max
    override def toString: String = name
  end Shape

  val chamberWidth = 7
  extension (c: Chamber)
    def maxY(): Int = c.map(_.y).maxOption.getOrElse(0)
    def contains(shape: Shape): Boolean = shape.points.exists(p => c.contains(p))


  // origin (0, 0) is at the left bottom of the shape
  val minus = Shape("-", Set(Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(3, 0)))
  val cross = Shape("+", Set(Coord(1, 0), Coord(0, 1), Coord(1, 1), Coord(2, 1), Coord(1, 2)))
  val mirroredl = Shape("\u2143", Set(Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(2, 1), Coord(2, 2)))
  val stick = Shape("|", Set(Coord(0, 0), Coord(0, 1), Coord(0, 2), Coord(0, 3)))
  val box = Shape("🔲", Set(Coord(0, 0), Coord(0, 1), Coord(1, 0), Coord(1, 1)))
  val empty = Shape("X", Set.empty)

  val jets = input.head.toCharArray.toList
  val jetStream = LazyList.from(0).map(ix => jets(ix % jets.size))

  val shapes = List(minus, cross, mirroredl, stick, box)
  val shapeStream = LazyList.from(0).map(ix => shapes(ix % shapes.size))

  // test inputs
  println(shapeStream.take(shapes.size).toList)
  println(jets.mkString(" "))

  def step( current: (Set[Coord], JetStream), nextShape: Shape): (Set[Coord], JetStream) =
    val (chamber, jet) = current
    val initMove = startMove.dy(chamber.maxY() + (if chamber.isEmpty then 1 else 2))
    val shape = nextShape.move(initMove)
    val (rest, nextJet) = fall(chamber, shape, jet)
    (rest.points ++ chamber, nextJet)
  end step

  @annotation.tailrec
  def fall(chamber: Set[Coord], shape: Shape, jet: JetStream): (Shape, JetStream) =
    // if falling down would be blocked then end of recursion
    val nextDown = shape.move(downMove)
    if nextDown.minY() < 0 || chamber.contains(nextDown) then (shape, jet)
    else {
      // check if can be moved left or right
      val nextAside = jet.head match {
        case '<' => nextDown.move(leftMove)
        case '>' => nextDown.move(rightMove)
      }
      val moved = if (nextAside.minX() < 0 || nextAside.maxX() >= chamberWidth || chamber.contains(nextAside)) nextDown
      else nextAside
      //println(jet.head)
      //debug(chamber, moved)
      fall(chamber, moved, jet.tail)
    }
  end fall

  def debug(chamber: Set[Coord], shape: Shape = empty): Unit =
    for (cy <- 10 to -1 by -1) {
      for (cx <- -1 to chamberWidth) {
        val ascii =
          if (cx == -1 || cx == chamberWidth) && cy >= 0 then s"${Console.BLUE}⎪${Console.RESET}"
          else if cy < 0 && cx > -1 && cx < chamberWidth then s"${Console.BLUE}⎺${Console.RESET}"
          else if chamber.contains(Coord(cx, cy)) then s"${Console.YELLOW_B}#${Console.RESET}"
          else if shape.points.contains(Coord(cx, cy)) then s"${Console.GREEN_B}@${Console.RESET}"
          else s"${Console.BLUE}.${Console.RESET}"
        print(ascii)
      }
      println()
    }
  end debug

  val last = shapeStream.take(2022).foldLeft((Set.empty[Coord], jetStream))(step)
  debug(last._1)

  val res1 = last._1.maxY() + 1
  println(s"res1: $res1") // 3109 (3068)


