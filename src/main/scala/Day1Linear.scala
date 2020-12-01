import scala.io.Source

object Day1Linear extends App {

  val solution = Source
    .fromResource("input1.txt")
    .getLines()
    .map(_.trim)
    .map(_.toInt)
    .foldLeft((Set.empty[Int], Set.empty[Int])) { (accu, i) =>
      val (seen, solutions) = accu
      val other = 2020 - i
      val newSolutions = if (seen.contains(other)) solutions + i else solutions
      (seen + i, newSolutions)
    }
    ._2
    .map(s => s * (2020 - s))
    .head
  println(s"solution=$solution")
}
