package puzzles.hackerrank

// https://www.hackerrank.com/challenges/kangaroo
object NumberLineJumps extends App {

  // x1 + v1 * x = x2 + v2 * x
  // x = (x1 - x2) / (v2 - v1)
  // return yes or no
  def kangaroo(x1: Int, v1: Int, x2: Int, v2: Int): String = {
    if (v1 == v2 && x1 != x2) "NO"
    else if (((x1 - x2) % (v2 - v1) == 0) && ((x1 - x2) / (v2 - v1) > 0)) "YES"
    else "NO"
  }

  println(s"meets=${kangaroo(0, 2, 5, 3)}")
}
