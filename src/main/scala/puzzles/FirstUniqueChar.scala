package puzzles

// Given a string s, find the first non-repeating character in it and return its index. If it does not exist, return -1.
// https://leetcode.com/problems/first-unique-character-in-a-string/
object FirstUniqueChar extends App {

  def firstUniqChar(s: String): Int = {
    val histo = s.groupMapReduce(identity)(_ => 1)(_ + _)
    s.find(histo(_) == 1).map(s.indexOf(_)).getOrElse(-1)
  }

  println(firstUniqChar("aaaabbbcddd"))
}
