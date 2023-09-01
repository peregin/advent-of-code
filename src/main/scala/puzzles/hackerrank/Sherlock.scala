package puzzles.hackerrank

// Sherlock and the valid strings:
// https://www.hackerrank.com/challenges/sherlock-and-valid-string/
object Sherlock extends App {

  // YES or NO
  // all values should be the same, or only one different with +1
  def isValid(s: String): String = {
    val histogram = s.groupMapReduce(identity)(_ => 1)(_ + _).values.toList.sorted
    histogram match {
      case Nil                                                                                     => "YES" // specs?
      case _ :: Nil                                                                                => "YES"
      case list :+ last if list.distinct.size == 1 && (list.head == last || list.head == last - 1) => "YES"
      case first :: list if list.distinct.size == 1 && (list.head == first || first == 1)          => "YES"
      case _                                                                                       => "NO"
    }
  }

  val test = "abcdefghhgfedecba" // or "abcc" or "aabbc"
  println(s"$test is ${isValid(test)}")
}
