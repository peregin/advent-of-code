package aoc.aoc2022

import aoc.Aoc

object Day10 extends aoc.Aoc("aoc2022/input10.txt", identity):

  sealed class Instruction(val duration: Int)
  case object Noop extends Instruction(duration = 1)
  case class Add(x: Long) extends Instruction(duration = 2)
  case class Step(remaining: Int, op: Instruction):
    def tick(): Step = this.copy(remaining = remaining - 1)

  val program = input.map{
      case "noop" => Noop
      case s"addx $x" => Add(x.toLong)
    }.map(instr => Step(instr.duration, instr))

  @annotation.tailrec
  def cycles(x: Long, program: List[Step], active: Option[Step], accu: List[Long]): List[Long] = {
    if program.isEmpty && active.isEmpty then accu :+ x
    else {
      val (checkNow, runLater) =
        if active.isEmpty then (program.headOption, program.tail)
        else (active, program)

      val (execNow, execLater) = checkNow.map(_.tick()) match {
        case me @ Some(s) if s.remaining <= 0 => (me, None)
        case other => (None, other)
      }

      val newX = x + execNow.map(_.op).map{
        case Add(v) => v
        case _ => 0L
      }.sum
      cycles(newX, runLater, execLater, accu :+ x)
    }
  }

  val res = cycles(1, program, None, Nil)
  val ixs = List(20, 60, 100, 140, 180, 220)
  val res1 = ixs.map(ix => res(ix-1) * ix).sum
  println(s"res1: $res1") // 13920

  val crt = res.sliding(40, 40).map(_.zipWithIndex.map((x, ix) => if ix >= x - 1 && ix <= x + 1 then "##" else "..").mkString)
  crt.foreach(println) // EGLHBLFJ



