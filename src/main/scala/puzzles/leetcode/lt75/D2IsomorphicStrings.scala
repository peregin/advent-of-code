package puzzles.leetcode.lt75

object D2IsomorphicStrings extends App {

  def isIsomorphic(s: String, t: String): Boolean = {
    val o1 = s.zip(t).groupBy(_._1)
    val c1 = o1.forall{ on =>
      val on1 = on._2.toSet
      on1.size == 1
    }
    val c2 = s.zip(t).groupBy(_._2).forall(_._2.toSet.size == 1)
    c1 && c2
  }

  println(isIsomorphic("papap", "titii"))       // f
  println(isIsomorphic("egg", "add"))           // t
  println(isIsomorphic("foo", "bar"))           // f
  println(isIsomorphic("paper", "title"))       // t
  println(isIsomorphic("bbbaaaba", "aaabbbba")) // f
  println(isIsomorphic("abab", "baba"))         // t
  println(isIsomorphic("a", "a"))               // t
  println(isIsomorphic("aaaa", "aaaa"))         // t
  println(isIsomorphic("badc", "baba"))         // f
}
