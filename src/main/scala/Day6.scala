

object Day6 extends Aso("input6_1.txt", identity) {

  val groups = input.foldLeft(List.empty[List[Set[Char]]])((accu, line) => accu match {
    case _ if line.isEmpty => Nil +: accu
    case Nil => List(List(line.toSet))
    case head :: rest => (head :+ line.toSet) +: rest
  })

  val solution1 = groups.map(_.reduce(_ ++ _)).map(_.size).sum
  println(s"solution1=$solution1")

  val solution2 = groups.map(_.reduce(_ intersect _)).map(_.size).sum
  println(s"solution2=$solution2")
}
