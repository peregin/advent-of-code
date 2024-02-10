package puzzles.hackerrank

/**
 * https://www.hackerrank.com/challenges/library-fine/problem
 */
object LibraryFine extends App {

  // d1 return date
  // d2 due date
  def libraryFine(d1: Int, m1: Int, y1: Int, d2: Int, m2: Int, y2: Int): Int = {
    if (y1 > y2) 10000
    else if (y1 < y2) 0
    else {
      if (m1 > m2) 500 * (m1 - m2)
      else if (m1 < m2) 0
      else {
        if (d1 > d2) 15 * (d1 - d2)
        else 0
      }
    }
  }
}
