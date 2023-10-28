package puzzles.hackerrank

/**
 * https://www.hackerrank.com/challenges/strange-advertising/problem
 *
 * Day Shared Liked Cumulative
 *  1      5     2       2
 *  2      6     3       5
 *  3      9     4       9
 *  4     12     6      15
 *  5     18     9      24
 */
object ViralAdvertising extends App {

  def viralAdvertising(n: Int): Int = {
    case class Day(shared: Int, liked: Int, cum: Int)

    val res = (1 until n).foldLeft(Day(5, 2, 2)) { case (day, ix) =>
      val shared = day.liked * 3
      val liked  = math.floor(shared / 2).toInt
      val next   = Day(shared, liked, day.cum + liked)
      // println(s"next=$next")
      next
    }
    // println(s"day=$res")
    res.cum
  }

  println(s"cumulative: ${viralAdvertising(3)}")
}
