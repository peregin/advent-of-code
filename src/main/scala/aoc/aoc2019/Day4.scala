package aoc.aoc2019

object Day4 extends App {

  def valid(p: Int): Boolean = {
    val s = p.toString.sliding(2).toList
    s.forall(f => f.charAt(0) <= f.charAt(1)) && s.exists(f => f.charAt(0) == f.charAt(1))
  }

  def group(a: Char, rest: String, curr: String, accu: List[String] = Nil): List[String] = {
    rest.toList match {
      case Nil => accu :+ (curr :+ a)
      case h :: t if a == h => group(h, t.mkString, curr :+ h, accu)
      case h :: t => group(h, t.mkString, "", accu :+ (curr :+ a))
    }
  }

  def valid2(p: Int): Boolean = {
    val t = p.toString
    val s = t.sliding(2).toList
    s.forall(f => f.charAt(0) <= f.charAt(1)) && group(t.head, t.tail, "", List.empty).exists(_.size == 2)
  }

  def count(from: Int, end: Int, check: Int => Boolean): Int = (from to end).map(check).count(_ == true)

  val res1 = count(171309, 643603, valid)
  println(res1) // 1625

  println(group('1', "23444", "", List.empty))

  val res2 = count(171309, 643603, valid2)
  println(res2) // ?

}
