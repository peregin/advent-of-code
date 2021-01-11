package aoc.aoc2020

import aoc.Aoc

// option 1 - scala parser combinators
// option 2 - tokenizer and stack/rpn
object Day18 extends Aoc("aso2020/input18.txt", identity) {

  sealed trait Token
  case class Digit(d: Long) extends Token {
    def +(that: Digit)      = Digit(this.d + that.d)
    def *(that: Digit)      = Digit(this.d * that.d)
    override def toString() = d.toString
  }
  case object OpeningPar extends Token {
    override def toString() = "("
  }
  case object ClosingPar extends Token {
    override def toString() = "("
  }
  case object Plus extends Token {
    override def toString() = "+"
  }
  case object Mult extends Token {
    override def toString() = "*"
  }

  def tokenize(expr: String): List[Token] =
    expr.toList.filter(_ != ' ').map {
      case '('            => OpeningPar
      case ')'            => ClosingPar
      case '+'            => Plus
      case '*'            => Mult
      case d if d.isDigit => Digit(d.asDigit.toLong)
      case other          => sys.error(s"unknown token $other")
    }

  // reverse polish notation
  def rpn(tokens: List[Token], precedence: Map[Token, Int]): List[Token] = {
    val accu = collection.mutable.Queue[Token]()
    val ops  = collection.mutable.Stack[Token]()
    tokens.foreach { t =>
      t match {
        case OpeningPar =>
          ops.push(t)
        case ClosingPar =>
          while (ops.head != OpeningPar) accu += ops.pop()
          ops.pop() // remove opening
        case d @ Digit(_) =>
          accu += d
        case o @ (Plus | Mult) =>
          while (ops.nonEmpty && precedence(ops.head) >= precedence(t)) accu += ops.pop()
          ops.push(o)
      }
    //println(s"t=$t - ${accu.mkString} s = $ops")
    }
    accu.toList ++ (if (precedence(Plus) > precedence(Mult)) ops.toList else ops.toList.reverse)
  }

  def postfix(rpn: List[Token]): Long = {
    val stack = collection.mutable.Stack[Token]()
    rpn.foreach { t =>
      t match {
        case Plus =>
          val a = stack.pop().asInstanceOf[Digit]
          val b = stack.pop().asInstanceOf[Digit]
          stack.push(a + b)
        case Mult =>
          val a = stack.pop().asInstanceOf[Digit]
          val b = stack.pop().asInstanceOf[Digit]
          stack.push(a * b)
        case t: Token =>
          stack.push(t)
      }
    }
    stack.pop().asInstanceOf[Digit].d
  }

  def eval(expr: String, precedence: Map[Token, Int]): Long = {
    val tokens = tokenize(expr)
    val stack  = rpn(tokens, precedence)
    postfix(stack)
  }

  //val test = "5 + (8 * 3 + 9 + 3 * 4 * 3)" // 437
  // 2 * 3 + (4 * 5) =

  // the AOC is special, + and * has the same precedence
  val precedence1 = Map[Token, Int](
    OpeningPar -> 0,
    Plus       -> 1,
    Mult       -> 1
  )
  val solution1 = input.map(eval(_, precedence1)).sum
  println(s"solution1 = $solution1")

  // the AOC part 2 is special, + has precedence over *
  val precedence2 = Map[Token, Int](
    OpeningPar -> 0,
    Plus       -> 2,
    Mult       -> 1
  )
  val solution2 = input.map(eval(_, precedence2)).sum
  println(s"solution2 = $solution2")
}
