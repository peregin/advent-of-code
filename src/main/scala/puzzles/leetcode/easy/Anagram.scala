package puzzles.leetcode.easy

object Anagram extends App {

  def isAnagram(s: String, t: String): Boolean = {
    s.sorted == t.sorted
  }

  println(isAnagram("anagram", "nagaram"))
}
