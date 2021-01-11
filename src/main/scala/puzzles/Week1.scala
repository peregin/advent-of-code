package puzzles

// Sherlock and the valid strings
object Week1 extends App {

  // YES or NO
  def isValid(s: String): String = {
    val histo = s.groupBy(c => c).mapValues(_.size)
    // all values should be the same, or only one different with +1
    "YES"
  }

  val test = "abc"
  println(s"$test is ${isValid(test)}")
}
