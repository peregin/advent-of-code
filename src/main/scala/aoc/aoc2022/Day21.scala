package aoc.aoc2022

import aoc.Aoc

object Day21 extends aoc.Aoc("aoc2022/input21.txt", identity):

  sealed abstract trait Expr:
    def eval(): Long

  case class Mul(m1: String, m2: String) extends Expr:
    override def eval(): Long = monkeys(m1).eval() * monkeys(m2).eval()
  case class Add(m1: String, m2: String) extends Expr:
    override def eval(): Long = monkeys(m1).eval() + monkeys(m2).eval()
  case class Min(m1: String, m2: String) extends Expr:
    override def eval(): Long = monkeys(m1).eval() - monkeys(m2).eval()

  case class Div(m1: String, m2: String) extends Expr:
    override def eval(): Long = monkeys(m1).eval() / monkeys(m2).eval()

  case class Num(n: Long) extends Expr:
    override def eval(): Long = n

  val monkeys = input.map {
    case s"$id: $m1 * $m2" => id -> Mul(m1, m2)
    case s"$id: $m1 + $m2" => id -> Add(m1, m2)
    case s"$id: $m1 - $m2" => id -> Min(m1, m2)
    case s"$id: $m1 / $m2" => id -> Div(m1, m2)
    case s"$id: $n"        => id -> Num(n.toInt)
  }.toMap

  val res1 = monkeys("root").eval()
  println(s"res1: $res1")
