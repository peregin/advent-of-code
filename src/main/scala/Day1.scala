import scala.io.Source

object Day1 extends App {

  val solution = Source
    .fromResource("input1.txt")
    .getLines()
    .map(_.trim)
    .map(_.toInt)
    .toList
    .combinations(2)
    .filter(_.sum == 2020)
    .map(_.product)
    .next()
  println(s"solution=$solution")
}
