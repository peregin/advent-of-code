import scala.io.Source

object Day6 extends App {

  val input = Source
    .fromResource("input6.txt")
    .getLines()
    .map(_.trim)
    .toList

  val groups = input.foldLeft(List.empty[List[Set[Char]]])((accu, line) => accu match {
    case Nil => List(List(line.toSet))
    case _ if line.isEmpty => List(Set.empty[Char]) +: accu
    case head :: rest if head.forall(_.isEmpty) => List(line.toSet) +: rest
    case head :: rest => (head :+ line.toSet) +: rest
  })

  val solution1 = groups.map(_.reduce(_ ++ _)).map(_.size).sum
  println(s"solution1=$solution1")

  val solution2 = groups.map(_.reduce(_ intersect _)).map(_.size).sum
  println(s"solution2=$solution2")
}
