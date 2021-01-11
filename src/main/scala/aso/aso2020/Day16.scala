package aso.aso2020

import aso.Aso

object Day16 extends Aso("aso2020/input16_2.txt", identity) {

  case class MinMax(min: Int, max: Int) {
    def valid(v: Int): Boolean = v >= min && v <= max
  }

  case class Result(rules: List[List[MinMax]], myTickets: List[Int], nearbyTickets: List[List[Int]])

  def parse(input: List[String], result: Result, stage: Int): Result =
    if (input.isEmpty) result
    else {
      val line = input.head
      stage match {
        case 0 if line.isEmpty => parse(input.tail, result, 1)
        case 0 =>
          val rlines = line.split(':').map(_.trim)
          val nextResult = result.copy(rules =
            result.rules :+ rlines(1)
              .replace("or ", "")
              .split(' ')
              .map { r =>
                val mm = r.split('-').map(_.toInt)
                MinMax(mm(0), mm(1))
              }
              .toList
          )
          parse(input.tail, nextResult, 0)
        case 1 if line.isEmpty                     => parse(input.tail, result, 2)
        case 1 if line.contains("your ticket:")    => parse(input.tail, result, 1)
        case 1                                     => parse(input.tail, result.copy(myTickets = line.split(',').map(_.toInt).toList), 1)
        case 2 if line.contains("nearby tickets:") => parse(input.tail, result, 2)
        case 2 =>
          parse(input.tail, result.copy(nearbyTickets = result.nearbyTickets :+ line.split(',').map(_.toInt).toList), 2)
      }
    }

  val notes  = parse(input, Result(Nil, Nil, Nil), 0)
  val ranges = notes.rules.flatten
  val invalids = (notes.myTickets ++ notes.nearbyTickets.flatten).map { t =>
    val v = ranges.exists(_.valid(t))
    if (v) None else Some(t)
  }.flatten

  val solution1 = invalids.sum
  println(s"solution1 = $solution1")
}
