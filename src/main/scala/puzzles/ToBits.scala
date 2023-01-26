package puzzles

object ToBits extends App:

  val nums = List(6, 32, 31)
  val res = nums.map(_.toBinaryString.reverse.padTo(16, '0').reverse).foldLeft("")(_ + _)
  println(s"res: $res")

