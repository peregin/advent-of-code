package aoc.aoc2023

import aoc.Aoc

import scala.collection.parallel.CollectionConverters.*

object Day6 extends aoc.Aoc("aoc2023/input6.txt", identity):

  def extract(s: String): List[Int] = s.split(':').last.split(' ').map(_.trim).filter(_.nonEmpty).map(_.toInt).toList

  def countWins(time: Long, distance: Long, min: Long, max: Long): Long =
    longStreamFrom(min).takeWhile(_ <= max).par.count(i => i * (time - i) > distance)

  def countWinsQuadraticFormula(time: Long, distance: Long): Long = {
    val a               = -1
    val b               = time
    val c               = -distance
    val sqrtDisc        = Math.sqrt(b * b - 4 * a * c)
    val maxMilliseconds = Math.floor(((-b - sqrtDisc) / (2 * a)) - 0.0001)
    val minMilliseconds = Math.ceil(((-b + sqrtDisc) / (2 * a)) + 0.0001)
    (maxMilliseconds - minMilliseconds + 1).toLong
  }

  val times     = extract(input.head)
  val distances = extract(input.last)
  val races     = times.zip(distances)
  println(races.mkString(","))
  val wins = races.map(r => countWins(r._1, r._2, 1, r._1 - 1))
  val res1 = wins.product
  println(s"res1: $res1")

  val oneTime     = times.mkString.toLong
  val oneDistance = distances.mkString.toLong
  val res21       = countWins(oneTime, oneDistance, 14, 71516)
  println(s"res21: $res21")
  val res2 = countWinsQuadraticFormula(oneTime, oneDistance)
  println(s"res2: $res2")
