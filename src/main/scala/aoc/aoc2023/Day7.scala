package aoc.aoc2023

import aoc.Aoc

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicLong
import scala.collection.parallel.CollectionConverters.*

object Day7 extends aoc.Aoc("aoc2023/input7_1.txt", identity):

  case class Hand(cards: String, bid: Int, cardType: Int)

  val res1 = 1
  println(s"res1: $res1")

  val res2 = 2
  println(s"res2: $res2")
