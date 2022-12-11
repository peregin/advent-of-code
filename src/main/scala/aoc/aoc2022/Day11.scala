package aoc.aoc2022

import aoc.Aoc

import scala.collection.mutable

// monkey business 🤣
object Day11 extends aoc.Aoc("aoc2022/input11.txt", identity):

  // monkey ledger 🧮
  type Ledger = Map[Int, Monkey]

  extension (s: Ledger)
    def result: Long = s.values.map(_.inspected).toList.sorted.takeRight(2).product

  case class Monkey(ix: Int, items: List[Long],
                    op: Long => Long, testDiv: Long, throwTrue: Int, throwFalse: Int,
                    inspected: Long)

  extension (s: String)
    def numberAfterY: Int = s.split('y')(1).trim.stripSuffix(":").toInt


  // parse it into an ADT
  val monkeys = splitByEmptyLine(input).map{lines =>
    // first line is the index
    val ix = lines(0).numberAfterY
    val items = lines(1).split(':')(1).split(',').map(_.trim.toLong).toList
    val op = lines(2) match {
      case "Operation: new = old + old" => (x: Long) => x * 2
      case "Operation: new = old * old" => (x: Long) => x * x
      case s"Operation: new = old + $num" => (x: Long) => x + num.toInt
      case s"Operation: new = old * $num" => (x: Long) => x * num.toInt
    }
    val testDiv = lines(3).numberAfterY
    val throwTrue = lines(4).numberAfterY
    val throwFalse = lines(5).numberAfterY
    Monkey(ix, items, op, testDiv, throwTrue, throwFalse, 0L)
  }

  val ix2Monkey = monkeys.map(m => m.ix -> m).toMap
  val ix = monkeys.map(_.ix)
  // when testing for remainder to throw to another monkey,
  // we could reduce the size of item kept by the least common multiple
  // (for simplicity, if if all are primes, just the product of all)
  val lcm = monkeys.map(_.testDiv).product

  def step(worry: Boolean)(ledger: Ledger, ix: Int): Ledger = {
    val m = ledger(ix)
    m.items.foldLeft(ledger){(accu, item) =>
      val opRes = m.op(item)
      val res = if worry then opRes % lcm else opRes / 3
      val throwIx = if res % m.testDiv == 0 then m.throwTrue else m.throwFalse
      accu.updatedWith(throwIx)(_.map(m => m.copy(items = m.items :+ res)))
    }.updatedWith(ix)(_.map(m => m.copy(items = Nil, inspected = m.inspected + m.items.size)))
  }

  def round(worry: Boolean)(ledger: Ledger, roundIx: Int): Ledger = ix.foldLeft(ledger)(step(worry))

  val round20 = (0 until 20).foldLeft(ix2Monkey)(round(worry = false))
  val res1 = round20.result
  println(s"res1: $res1") // 108240 (or 10605)

  val round10000 = (0 until 10000).foldLeft(ix2Monkey)(round(worry = true))
  val res2 = round10000.result
  println(s"res2: $res2") // 25712998901 (or 2713310158)



