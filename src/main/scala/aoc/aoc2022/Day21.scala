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

  case class Comp(m1: String, m2: String) extends Expr:
    override def eval(): Long = monkeys(m1).eval().compareTo(monkeys(m2).eval())


  val monkeys = collection.mutable.HashMap.from[String, Expr](input.map {
    case s"$id: $m1 * $m2" => id -> Mul(m1, m2)
    case s"$id: $m1 + $m2" => id -> Add(m1, m2)
    case s"$id: $m1 - $m2" => id -> Min(m1, m2)
    case s"$id: $m1 / $m2" => id -> Div(m1, m2)
    case s"$id: $n"        => id -> Num(n.toInt)
  })

  val res1 = monkeys("root").eval()
  println(s"res1: $res1") // 21120928600114 (152)

  val Add(m1, m2) = monkeys("root")
  monkeys.update("root", Comp(m1, m2))

  // revert operations or do a search, maybe memoize the intermediate results?
  // implementing binary search was faster 🤷
  @annotation.tailrec
  def search(min: Long, max: Long): Long =
    val attempt = (min + max) / 2
    monkeys.update("humn", Num(attempt))
    val check = monkeys("root").eval()
    if check == 0L then attempt
    else if max <= min then sys.error(s"not found $min <= $max")
    else
      // it is too late 🌙, to do it properly need to identify if humn is on the left or right side of the operation
      // for samples is on the left side for the real data is on the right side, so the comparison sign needs to be flipped
      val (newMin, newMax) = if check < 0 then (min, attempt) else (attempt, max)
      search(newMin, newMax)

  val res2 = search(0, 3453748220117L) // this was too high
  println(s"res2: $res2") // 3453748220116 too high (301)
