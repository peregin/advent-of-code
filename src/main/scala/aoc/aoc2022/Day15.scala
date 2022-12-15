package aoc.aoc2022

import aoc.Aoc
import aoc.aoc2022.Day14.Coord

object Day15 extends aoc.Aoc("aoc2022/input15.txt", identity):

  case class Coord(x: Int, y: Int):
    def manhattan(that: Coord): Int = (x - that.x).abs + (y - that.y).abs

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

  // detect and collect covered interval
  def detect(lines: List[Line], y: Int): List[Interval] =
    lines.foldLeft(List.empty[Interval]){ (accu, line) =>
      val dy = (y - line.sensor.y).abs
      val dx = line.distance - dy
      if dx < 0 then accu else Interval(line.sensor.x - dx, line.sensor.x + dx) +: accu
    }

    // merge overlapping intervals
  def merge(intervals: List[Interval]): List[Interval] = intervals.sortBy(_.min).foldLeft(List.empty[Interval])((accu, range) => accu match {
    case Nil => List(range)
    case a :: rest if a.overlaps(range) => a.merge(range) +: rest
    case _ => range +: accu
  })

  val ints = detect(lines, 2000000) // 10
  val merged = merge(ints)

  val res1 = merged.map(_.length).sum
  println(s"res1: $res1") // 5240818 (26)


