package puzzles.hackerrank


// https://www.hackerrank.com/challenges/reduced-string
object SuperReducedString extends App {

  def superReducedString(s: String): String = {
    def reduce(acc: List[Char]): List[Char] = acc match {
      case Nil => Nil
      case x :: Nil => x :: Nil
      case x :: y :: xs if x == y => reduce(xs)
      case x :: y :: xs => x :: reduce(y :: xs)
    }

    reduce(s.toList).mkString("")
  }

  //println(s"[${superReducedString("aa")}]")
  println(s"[${superReducedString("baab")}]")
}
