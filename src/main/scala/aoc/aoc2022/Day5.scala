package aoc.aoc2022

import aoc.Aoc

object Day5 extends aoc.Aoc("aoc2022/input5.txt", identity):
  override protected def shouldTrim = false // don not drop leading spaces

  val split = splitByEmptyLine(input)
  val setup = split.head.dropRight(1) // drop empty separator
  val instructions = split.last

  val max = setup.map(_.length).max
  val crates = setup
    .map(_.padTo(max, ' ')) // to be able to transpose
    .map(line => (1 to line.length by 4).map(line))
    .transpose
    .map(_.mkString.trim)
  println(s"crates=$crates")

  case class Move(count: Int, from: Int, to: Int)
  val moves = instructions.map{
    case s"move $count from $from to $to" => Move(count.toInt, from.toInt - 1, to.toInt - 1)
  }
  println(s"moves=$moves")

  def move1(c: List[String], m: Move): List[String] = {
    val (moveStack, stayStack) = c(m.from).splitAt(m.count)
    c.updated(m.from, stayStack).updated(m.to, moveStack.reverse ++ c(m.to))
  }

  val res1 = moves.foldLeft(crates)(move1).map(_.head).mkString
  println(s"res1: $res1")

  def move2(c: List[String], m: Move): List[String] = {
    val (moveStack, stayStack) = c(m.from).splitAt(m.count)
    c.updated(m.from, stayStack).updated(m.to, moveStack ++ c(m.to))
  }

  val res2 = moves.foldLeft(crates)(move2).map(_.head).mkString
  println(s"res2: $res2")


