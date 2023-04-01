package puzzles.gc

import scala.io.Source
import scala.io.StdIn.readLine

object AllocationHouse2020 extends App {

  val cases = readLine().toInt
  (0 until cases).foreach{ i =>
    val ins = readLine().split(' ')
    val n = ins(0).toInt
    val budget = ins(1).toInt
    val houses = readLine().split(' ').map(_.toInt).toList
    assert(n == houses.length)

    def count(list: List[Int], b: Int): Int = {
      if (list.isEmpty) 0
      else {
        val f = list.head
        if (b >= f) 1 + count(list.tail, b-f) else 0
      }
    }

    val h = count(houses.sorted, budget)
    println(s"Case #$i: $h")
  }
}
