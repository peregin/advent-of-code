package aoc.aoc2022

import aoc.Aoc
import aoc.aoc2022.Day14.Coord

object Day15 extends aoc.Aoc("aoc2022/input15.txt", identity):

  case class Coord(x: Int, y: Int):
    def manhattan(that: Coord): Int = (x - that.x).abs + (y - that.y).abs

    def tuningFrequency: Long = 4000000L * x + y

  case class Line(sensor: Coord, beacon: Coord, distance: Int)

  case class Interval(min: Int, max: Int):
    require(min <= max)

    lazy val length = max - min

    def overlaps(that: Interval): Boolean = if this.min < that.min then
      that.min <= this.max else this.min <= that.max

    def merge(that: Interval): Interval = Interval(this.min min that.min, this.max max that.max)

  val lines = input.map{
    case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
      val sensor = Coord(sx.toInt, sy.toInt)
      val beacon = Coord(bx.toInt, by.toInt)
      Line(sensor, beacon, sensor.manhattan(beacon))
  }

  /*
   * detect and collect covered interval
   *                 S1
   *    S2          /  \
   *   / \         /    \
   * -|===|-------|======|---------- on row y
   */
  def detect(lines: List[Line], y: Int): List[Interval] =
    lines.foldLeft(List.empty[Interval]){ (accu, line) =>
      val dy = (y - line.sensor.y).abs
      val dx = line.distance - dy
      if dx < 0 then accu else Interval(line.sensor.x - dx, line.sensor.x + dx) +: accu
    }

    // merge overlapping intervals, need to sort first on min  and the result will be in reversed order
  def merge(intervals: List[Interval]): List[Interval] = intervals.sortBy(_.min).foldLeft(List.empty[Interval])((accu, range) => accu match {
    case Nil => List(range)
    case a :: rest if a.overlaps(range) => a.merge(range) +: rest
    case _ => range +: accu
  })

  val merged = merge(detect(lines, 2000000)) // 10
  val res1 = merged.map(_.length).sum // sum of disjunct interval lengths
  println(s"res1: $res1") // 5240818 (26)

  @annotation.tailrec
  def find(area: Int, y: Int): Option[Coord] = {
    if y >= area then None
    else {
      val ints = merge(detect(lines, y)).filter(i => i.max >= 0 && i.min <= area).reverse
      if (ints.size == 2) Some(Coord(ints.head.max + 1, y)) // the missing only coord must be between the 2 intervals
      else find(area, y + 1) // otherwise check the rest
    }
  }
  val res2 = find(4000000, 0).tapEach(println).map(_.tuningFrequency) // 20
  println(s"res2: $res2") // 13213086906101 (56000011)


