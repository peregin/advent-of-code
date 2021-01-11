package aso.aso2020

import aso.Aso

import scala.math.sqrt

/**
  * 939
  * 7,13,x,x,59,x,31,19
  */
object Day13 extends Aso("aso2020/input13.txt", identity) {

  val now   = input(0).toLong
  val notes = input(1).split(',')
  println(s"buses=${notes.size}")
  val buses = notes.filter(_ != "x").map(_.toLong)
  val zero  = 0L

  def nextDeparture(bus: Long, ts: Long): Long = {
    val unit   = ts / bus
    val modulo = ts % bus
    unit * bus + (if (modulo > 0) bus else zero)
  }

  // find next departures
  val next      = buses.map(bus => (bus, nextDeparture(bus, now))).minBy(_._2)
  val solution1 = next._1 * (next._2 - now)
  println(s"solution1=$solution1")

  def stream(i: Long, step: Long = 1): LazyList[Long] = i #:: stream(i + step, step)

  def subsequent(ts: Long): Long = {
    val first   = nextDeparture(buses.head, ts)
    val pattern = notes.zipWithIndex.map { case (n, ix) => (n, first + ix) }.filter(_._1 != "x").map(_._2)
    // ignore first, already checked as first departure
//    println(s"buses=${buses.mkString("|")}")
//    println(s"departures=${departures.mkString("|")}")
//    println(s"pattern=${pattern.mkString("|")}")
    // might have gaps, see 457 -> 13
    if (pattern.zip(buses).tail.forall { case (n, bus) => n % bus == 0 }) first else zero
  }

  def isPrime(num: Long): Boolean = (num > 1) && !(2 to sqrt(num).toInt).exists(x => num % x == 0)
  val primes                      = buses.map(isPrime)
  println(s"primes=${primes.mkString("|")}")

  //val test = subsequent(1068781))
  //println(s"test=$test")

  // brute force
//  val solution2 = stream(100000000000000L).map{n =>
//  if (n % 1000000 == 0) println(s"$n")
//    subsequent(n)
//  }.find(_ > 0)

// after failing with brute force, work with chinese remainder theorem
// ------------------------------------------
  // chinese remainder theorem
  // https://en.wikipedia.org/wiki/Chinese_remainder_theorem
  // https://www.youtube.com/watch?v=zIFehsBHB8o

  // x + ix = 0 % bus
  // x = -ix % bus <= chinese remainder theorem
  // find all x, where (see youtube video)
  //
  // x = 3 % 5
  // x = 1 % 7
  // x = 6 % 8
  // note that 5, 7, 8 are relatively primes, but all the buses are prime numbers
  // x = 78

  val list = notes.zipWithIndex.filter(_._1 != "x").map { case (n, ix) => (n.toLong, ix) }
  val p    = list.map(_._1).product
  val s = list.map {
    case (bus, ix) =>
      val d = p / bus
      // https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
      ix * d * BigInt(d).modInverse(bus)
  }.sum
  val solution2 = p - s % p
  println(s"solution2=$solution2")
}
