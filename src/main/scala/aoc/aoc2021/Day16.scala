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
  }
  case class Literal(version: Int, value: Long) extends Packet
  case class Operator(version: Int, children: List[Packet]) extends Packet

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
    if typeId == 4 then readLiteral(bits).pipe((value, tail) => (Literal(version, value), tail))
    else bits match {
      case s"0$rest" =>
        val (l, tail) = rest.splitAt(15)
        val length = bin2Int(l)
        tail.splitAt(length).pipe((chunk, rest) => (Operator(version, readChunk(chunk)), rest))
      case s"1$rest" =>
        val (n, tail) = rest.splitAt(11)
        val num = bin2Int(n)
        readN(tail, num).pipe((children, rest) => (Operator(version, children), rest))
    }

  def sumVersion(p: List[Packet]): Int = p.map{
    case Literal(v, _) => v
    case Operator(v, children) => v + sumVersion(children)
  }.sum

  //val res1 = decode(binary("D2FE28"))
  //val res1 = decode(binary("38006F45291200"))
  //val res1 = decode(binary("EE00D40C823060"))
  //val res1 = sumVersion(List(decode(binary("8A004A801A8002F478"))._1))
  val res1 = sumVersion(List(decode(binary(input.head))._1))
  println(s"res1=$res1")






