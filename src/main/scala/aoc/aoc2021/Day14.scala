package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec

object Day14 extends Aoc("aoc2021/input14.txt", identity):

  val (polymer, rules) = input.span(_.nonEmpty) match {
    case (templates, pairs) => (templates.head, pairs.filter(_.nonEmpty).map{
      case s"$a -> $b" => a -> (a.head + b + a.last)
    }.toMap)
  }
  println(s"polymer: $polymer") //, rules:\n${rules.mkString("\n")}")

  def enrich(s: String): String = rules.getOrElse(s, s)

  def extract(s: String): String = s.sliding(2, 1).map(enrich).foldLeft("")((accu, c) => accu.dropRight(1) + c)

  @tailrec
  def step(s: String, n: Int): String =
    if n == 0 then s
    else
      val next = extract(s)
      //println(s"step $n: $s -> $next")
      step(next, n - 1)

  def count(n: Int): Long =
    val p1 = step(polymer, n)
    println(s"length: ${p1.length}")
    val m1 = p1.groupMapReduce(identity)(_ => 1L)(_ + _).values.toList
    m1.max - m1.min

  val res1 = count(10)
  println(s"res1=$res1")

  val res2 = count(40)
  println(s"res2=$res2")