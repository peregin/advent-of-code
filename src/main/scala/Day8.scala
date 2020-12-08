import scala.annotation.tailrec
import scala.io.Source

/**
  * nop +0
  * acc +1
  * jmp +4
  * acc +3
  * jmp -3
  * acc -99
  * acc +1
  * jmp -4
  * acc +6
  */
object Day8 extends App {

  val input = Source
    .fromResource("input8.txt")
    .getLines()
    .map(_.trim)
    .toList

  abstract sealed class Cmd(val arg: Int)
  final case class Nop(override val arg: Int) extends Cmd(arg)
  final case class Acc(override val arg: Int) extends Cmd(arg)
  final case class Jmp(override val arg: Int) extends Cmd(arg)

  val instructions: List[Cmd] = input.map { in =>
    val list = in.split(' ').map(_.trim)
    val arg  = list(1).toInt
    list(0) match {
      case "nop" => Nop(arg)
      case "acc" => Acc(arg)
      case "jmp" => Jmp(arg)
      case other => throw new IllegalStateException(s"unknown cmd $other at state $this")
    }
  }

  case class State(commands: List[Cmd], nextPc: Int, ac: Int, executed: Set[Int]) {

    def next(): State = {
      val cmd = commands(nextPc)
      //println(s"executing $cmd")
      cmd match {
        case Nop(_)   => execute(0, 1)
        case Acc(arg) => execute(arg, 1)
        case Jmp(arg) => execute(0, arg)
      }
    }

    private def execute(delta: Int, jump: Int) =
      this.copy(nextPc = this.nextPc + jump, ac = this.ac + delta, executed = this.executed + this.nextPc)

    def isLast = nextPc >= commands.size
  }

  @tailrec
  def execute(curr: State): State = {
    val next = curr.next()
    if (curr.executed.contains(next.nextPc)) curr
    else if (next.isLast) next
    else execute(next)
  }

  val zero      = State(instructions, 0, 0, Set.empty)
  val solution1 = execute(zero).ac
  println(s"solution1=$solution1")

  val solution2 = (0 until instructions.size)
    .to(LazyList)
    .map { ix =>
      val swap = instructions(ix) match {
        case Nop(arg) => Jmp(arg)
        case Jmp(arg) => Nop(arg)
        case other    => other
      }
      instructions.updated(ix, swap)
    }
    .map { list =>
      val res = execute(State(list, 0, 0, Set.empty))
      res
    }
    .find(_.isLast)
    .get
    .ac
  println(s"solution2=$solution2")
}
