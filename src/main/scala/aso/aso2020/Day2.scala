package aso.aso2020

import scala.io.Source

// format looks like
// 6-10 z: zhzzzzfzzzzzzzzzpzz
object Day2 extends App {

  val pattern = "(\\d+)-(\\d+) ([a-zA-Z]): ([a-zA-Z]+)".r

  val solution = Source
    .fromResource("aso2020/input2.txt")
    .getLines()
    .map(_.trim)
    .map(_ match {
      case pattern(min, max, letter, pwd) => validate2(min.toInt, max.toInt, letter.charAt(0), pwd)
    })
    .count(_ == true)
  println(s"solution=$solution")

  def validate1(min: Int, max: Int, letter: Char, pwd: String): Boolean = {
    val occurences = pwd.count(_ == letter)
    occurences >= min && occurences <= max
  }

  def validate2(min: Int, max: Int, letter: Char, pwd: String): Boolean = {
    val c1 = pwd.charAt(min - 1)
    val c2 = pwd.charAt(max - 1)
    c1 != c2 && (letter == c1 || letter == c2)
  }
}
