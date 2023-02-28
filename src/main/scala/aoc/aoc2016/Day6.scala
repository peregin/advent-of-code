package aoc.aoc2016

object Day6 extends aoc.Aoc("aoc2016/input6.txt", identity) {



  val res1 = input.transpose.map(_.groupMapReduce(identity)(_ => 1)(_ + _).maxBy((_, f) => f)).map(_._1).mkString
  println(res1)

  val res2 = input.transpose.map(_.groupMapReduce(identity)(_ => 1)(_ + _).minBy((_, f) => f)).map(_._1).mkString
  println(res2)
}
