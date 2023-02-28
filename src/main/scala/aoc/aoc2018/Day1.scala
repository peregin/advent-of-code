package aoc.aoc2018

object Day1 extends aoc.Aoc("aoc2018/input1.txt", _.toInt) {

  val res1 = input.foldLeft(0)((accu, f) => accu + f)
  println(res1)

  def firstDup(f: Int, seen: Set[Int], list: List[Int], orig: List[Int]): Int = {
    //println(f)
    list match {
      case Nil => firstDup(f, seen, orig, orig)
      case h :: _ if seen.contains(h + f) => h + f
      case h :: rest => firstDup(f + h, seen + h, rest, orig)
    }
  }

  val on = List(3, 3, 4, -2, -4)
  val res2 = firstDup(0, Set.empty, on, on)
  println(res2)
}
