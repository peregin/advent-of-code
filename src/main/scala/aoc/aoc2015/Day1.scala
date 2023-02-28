package aoc.aoc2015

object Day1 extends aoc.Aoc("aoc2015/input1.txt", identity) {

  val in = input.head
  val res1 = in.foldLeft(0){
    case (accu, '(') => accu + 1
    case (accu, ')') => accu - 1
  }
  println(res1)

  val res2 = in.foldLeft(List(0)){
    case (accu, '(') => accu :+ (accu.last + 1)
    case (accu, ')') => accu :+ (accu.last - 1)
  }
  println(res2.indexOf(-1))
}
