package puzzles

// https://www.hackerrank.com/challenges/find-digits
object FindDigits extends App {

  def findDigits(n: Int): Int = {
    val digits = n.toString.map(_.asDigit)
    digits.count(d => d != 0 && n % d == 0)
  }
}
