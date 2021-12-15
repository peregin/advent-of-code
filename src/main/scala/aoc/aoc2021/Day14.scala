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

  def extract1(s: String): String =
    val in1 = s.sliding(2, 2).foldLeft(Array.empty[String])((accu, pair) => accu :+ enrich(pair).mkString)
    println(s"extract1: ${in1.mkString(",")}")
    val in2 = (in1 zip in1.tail).map{ case (a, b) =>
      a.dropRight(1) + enrich(List(a.last, b.head).mkString) + b.drop(1)
    }
    println(s"extract2: ${in2.mkString(",")}")
    in2.mkString

//    def extract(s: String, prev: Option[Char]): String =
//      s.toCharArray match {
//        case Array(a, b, rest @ _*) => rest.mkString
//      }


  @tailrec
  def step(s: String, n: Int): String =
    if n == 0 then s
    else
      val next = extract1(s)
      println(s"step $n: $s -> $next")
      step(next, n - 1)



  val p1 = step(polymer, 2)
  println(s"p1: ${p1.length}")
  val m1 = p1.groupMapReduce(identity)(_ => 1)(_ + _).values.toList
  val res1 = m1.max - m1.min

  println(s"res1=$res1")