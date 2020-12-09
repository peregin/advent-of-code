

object Day9 extends Aso("input9.txt", _.toLong) {

  val preamble = 25

  val solution1 = input
    .sliding(preamble + 1)
    .find { list =>
      val check  = list.last
      val window = list.dropRight(1)
      !window.combinations(2).exists(_.sum == check)
    }
    .get
    .last
  println(s"solution1=$solution1")

  val indices = for (
    i <- 0 to input.size;
    j <- i + 1 to input.size
  ) yield (i, j)
  val solution2 = indices.map{case (min, max) => input.slice(min, max)}.find(_.sum == solution1).map(l => l.min + l.max).get
  println(s"solution2=$solution2")
}
