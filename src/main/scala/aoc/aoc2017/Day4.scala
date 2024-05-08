package aoc.aoc2017

object Day4 extends aoc.Aoc("aoc2017/input4.txt", identity) {

  val res1 = input.map{line =>
    line.split(' ').groupMapReduce(identity)(_ => 1)(_ + _).values.forall(_ == 1)
  }.count(_ == true)
  println(s"res1=$res1") // 383

  val res2 = input.map { line =>
    line.split(' ').map(_.sorted).groupMapReduce(identity)(_ => 1)(_ + _).values.forall(_ == 1)
  }.count(_ == true)
  println(s"res2=$res2") // 265
}
