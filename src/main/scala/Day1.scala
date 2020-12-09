

object Day1 extends Aso("input1.txt", _.toInt) {

  val solution = input
    .combinations(2)
    .filter(_.sum == 2020)
    .map(_.product)
    .next()
  println(s"solution=$solution")
}
