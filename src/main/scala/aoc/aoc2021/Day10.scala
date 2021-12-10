package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day10 extends Aoc("aoc2021/input10.txt", _.toList):

  val closing = Map(
    ')' -> '(',
    ']' -> '[',
    '}' -> '{',
    '>' -> '<'
  )
  val opening = closing.map(_.swap)
  val points = Map(
    ')' -> (3, 1),
    ']' -> (57, 2),
    '}' -> (1197, 3),
    '>' -> (25137, 4)
  )

  def validate(in: List[Char], instack: List[Char] = List.empty): Either[Char, List[Char]] = (in, instack) match {
    case (first :: rest, stack) if opening.contains(first) => validate(rest, first :: stack)
    case (first :: rest, pop :: stack) if closing.get(first).exists(_ == pop) => validate(rest, stack)
    case (Nil, stack) => Right(stack)
    case (first :: rest, _) => Left(first)
  }

  //println(validate("{([(<{}[<>[]}>{[]{[(<()>".toList))

  val res1 = input.map(validate(_))
    .collect{ case Left(p) => p}
    .groupMapReduce(identity)(points(_)._1)(_ + _)
    .values
    .sum
  println(s"res1 = $res1")

  val scores = input.map(validate(_))
    .collect{ case Right(s) => s.map(opening).map(points(_)._2.toLong)}
    .map(_.foldLeft(0L)((total, p) => total * 5 + p))
  val res2 = scores.sorted.apply(scores.size / 2)
  println(s"res2 = $res2")



