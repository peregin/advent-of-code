package aoc.aoc2022

import aoc.Aoc

import java.util.StringTokenizer
import scala.jdk.CollectionConverters._

/**
 * [[4,4],4,4]
 * [[4,4],4,4,4]
 *
 * or
 *
 * [[1],[2,3,4]]
 * [[1],4]
 *
 */
object Day13 extends aoc.Aoc("aoc2022/input13.txt", identity):

  // left is `[` or `]`, right is the value, comma is ignored
  type Token = Either[Char, Int]

  def build(s: String): List[Token] = {
    val st =  new StringTokenizer(s, "[],", true).asIterator().asScala
    st.flatMap{
      case "[" => Some(Left('['))
      case "]" => Some(Left(']'))
      case "," => None
      case v: String => Some(Right(v.toInt))
      case _ => None
    }.toList
  }

  // pairs in a tuple
  val pairs = splitByEmptyLine(input).map(pair => (build(pair(0)), build(pair(1))))

  def compare(left: List[Token], right: List[Token]): Boolean = (left.head, right.head) match {
    case (l, r) if l == r => compare(left.tail, right.tail)
    case (Left(']'), _) => true // left side ran out of items, ok
    case (_, Left(']')) => false // right side ran out of items, not ok
    case (Left('['), v @ Right(_)) => compare(left.tail, List(v, Left(']')) ++ right.tail) // make it a single element list on the right side (opening bracket not needed)
    case (v @ Right(_), Left('[')) => compare(List(v, Left(']')) ++ left.tail, right.tail) // make it a single element list on the left side (opening bracket not needed)
    case (Right(l), Right(r)) => l < r // just compare it, that this stage is either greater or less (equalty was checked in the beginning)
  }

  val res1 = pairs.map(p => compare(p._1, p._2)).zipWithIndex.filter(_._1).map(_._2 + 1).sum
  println(s"res1: $res1") // // 5717 (13)

  // build a new list
  val divs = Set(build("[[2]]"), build("[[6]]"))
  val list = pairs.flatMap(p => List(p._1, p._2)) ++ divs
  val lookup = list.sortWith(compare).zipWithIndex.map((list, ix) => (divs.contains(list), ix + 1))
  val res2 = lookup.filter(_._1).map(_._2).product
  println(s"res2: $res2") // 25935 (140)




