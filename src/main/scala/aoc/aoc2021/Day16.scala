package aoc.aoc2021

import aoc.Aoc

import scala.annotation.tailrec
import scala.util.chaining.*

object Day16 extends Aoc("aoc2021/input16.txt", identity):

  def binary(s: String): String =
    s.map{h =>
      val bin = BigInt(h.toString, 16).toString(2)
      val pad = "0" * (4 - bin.length)
      pad + bin
    }.mkString

  def bin2Int(s: String): Int = Integer.parseInt(s, 2)
  def bin2Long(s: String): Long = java.lang.Long.parseLong(s, 2)

  sealed trait Packet {
    def version: Int
    def typeId: Int
  }
  case class Literal(version: Int, typeId: Int, value: Long) extends Packet
  case class Operator(version: Int, typeId: Int, children: List[Packet]) extends Packet

  def readLiteral(s: String, accu: List[String] = List.empty): (Long, String) = s match {
    case s"1$rest" =>
      val rem = rest.drop(4)
      val newAccu = accu :+ rest.take(4)
      readLiteral(rem, newAccu)
    case s"0$rest" =>
      val bits = (accu :+ rest.take(4)).flatten.mkString
      val value = bin2Long(bits)
      (value, rest.drop(4))
  }

  @tailrec
  def readChunk(s: String, accu: List[Packet] = List.empty): List[Packet] =
    if (s.isEmpty) accu
    else {
      val (p, rest) = decode(s)
      readChunk(rest, accu :+ p)
    }

  def readN(s: String, n: Int, accu: List[Packet] = List.empty): (List[Packet], String) =
    if n == 0 then (accu, s)
    else {
      val (p, rest) = decode(s)
      readN(rest, n - 1, accu :+ p)
    }


  def decode(bin: String): (Packet, String) =
    val (v, rest) = bin.splitAt(3)
    val version = bin2Int(v)
    val (t, bits) = rest.splitAt(3)
    val typeId = bin2Int(t)
    if typeId == 4 then readLiteral(bits).pipe((value, tail) => (Literal(version, typeId, value), tail))
    else bits match {
      case s"0$rest" =>
        val (l, tail) = rest.splitAt(15)
        val length = bin2Int(l)
        tail.splitAt(length).pipe((chunk, rest) => (Operator(version, typeId, readChunk(chunk)), rest))
      case s"1$rest" =>
        val (n, tail) = rest.splitAt(11)
        val num = bin2Int(n)
        readN(tail, num).pipe((children, rest) => (Operator(version, typeId, children), rest))
    }

  def sumVersion(p: List[Packet]): Int = p.map{
    case Literal(v, _, _) => v
    case Operator(v, _, children) => v + sumVersion(children)
  }.sum

  //val res1 = decode(binary("D2FE28"))
  //val res1 = decode(binary("38006F45291200"))
  //val res1 = decode(binary("EE00D40C823060"))
  //val res1 = decode(binary("8A004A801A8002F478"))
  val res1 = decode(binary(input.head)).pipe(p => sumVersion(List(p._1)))
  println(s"res1=$res1")

  def eval(p: Packet): Long = p match {
    case Literal(_, _, v) => v
    case Operator(_, 0, children) => children.map(eval).sum
    case Operator(_, 1, children) => children.map(eval).product
    case Operator(_, 2, children) => children.map(eval).min
    case Operator(_, 3, children) => children.map(eval).max
    case Operator(_, 5, children) => if eval(children.head) > eval(children(1)) then 1 else 0
    case Operator(_, 6, children) => if eval(children.head) < eval(children(1)) then 1 else 0
    case Operator(_, 7, children) => if eval(children.head) == eval(children(1)) then 1 else 0
  }

  //val test = "C200B40A82"
  //val test = "F600BC2D8F"
  val test = input.head
  val res2 = decode(binary(test)).pipe(p => eval(p._1))
  println(s"res2=$res2")






