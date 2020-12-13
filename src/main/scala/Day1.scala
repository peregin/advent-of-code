
object Day1 extends Aso("input1.txt", _.toInt) {

  val solution = input
    .combinations(2)
    .filter(_.sum == 2020)
    .map(_.product)
    .next()
  println(s"solution=$solution")
}

object Day1Linear extends Aso("input1.txt", _.toInt) {

  val solution = input
    .foldLeft((Set.empty[Int], Set.empty[Int])) { (accu, i) =>
      val (seen, solutions) = accu
      val other             = 2020 - i
      val newSolutions      = if (seen.contains(other)) solutions + i else solutions
      (seen + i, newSolutions)
    }
    ._2
    .map(s => s * (2020 - s))
    .head
  println(s"solution=$solution")
}
