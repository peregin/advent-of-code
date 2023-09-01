package puzzles.leetcode

import scala.annotation.tailrec

// Given a roman numeral, convert it to an integer.
// https://leetcode.com/problems/roman-to-integer/
object Roman2Int extends App {

  def romanToInt(s: String): Int = {
    val roman = Map(
      'I' -> 1,
      'V' -> 5,
      'X' -> 10,
      'L' -> 50,
      'C' -> 100,
      'D' -> 500,
      'M' -> 1000
    )
    // or an alternative solution using foldLeft and compare current vale with the next value
    // if bigger than deduct the current value otherwise sum up the current value
    @tailrec
    def collect(accu: Int, s: String): Int = s match {
        case "" => accu
        case s"IV$rest" => collect(accu + 4, rest)
        case s"IX$rest" => collect(accu + 9, rest)
        case s"XL$rest" => collect(accu + 40, rest)
        case s"XC$rest" => collect(accu + 90, rest)
        case s"CD$rest" => collect(accu + 400, rest)
        case s"CM$rest" => collect(accu + 900, rest)
        case other => collect(accu + roman(other.head), other.tail)
    }
    collect(0, s)
  }

  println(romanToInt("MCMXCIV"))
}
