package aoc.aoc2023

import aoc.Aoc

import scala.annotation.tailrec

object Day8 extends aoc.Aoc("aoc2023/input8.txt", _.split(' ').map(_.trim.toInt).toList):

  type Input = List[Int]
  
  enum Direction:
    case Forward, Backward

  def build(on: Input): List[Input] = build(on, List(on))
  @tailrec
  private def build(on: Input, accu: List[Input]): List[Input] =
    val last = accu.last
    if (last.forall(_ == 0)) accu
    else build(on, accu :+ last.tail.zip(last).map(_ - _))

  @tailrec
  def predict(on: List[Input], direction: Direction, accu: List[Input] = Nil): List[Input] =
    if (on.isEmpty) accu
    else {
      val last = on.last
      val next = direction match {
        case Direction.Forward =>
          if (accu.isEmpty) last :+ 0
          else last :+ (accu.head.last + last.last)
        case Direction.Backward =>
          if (accu.isEmpty) 0 +: last
          else (last.head - accu.head.head) +: last
      }
        
      predict(on.dropRight(1),direction,  next +: accu)
    }

  val res1 = input.map(on => predict(build(on), Direction.Forward).head.last).sum
  println(s"res1: $res1") // 2105961943
  
  val res2 = input.map(on => predict(build(on), Direction.Backward).head.head).sum
  println(s"res2: $res2") // 1019
