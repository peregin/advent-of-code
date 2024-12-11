package aoc.aoc2024

object Day11 extends App:

  // keep the numbers as key and values as frequencies, the original brute force doesn't work with 75 iterations
  type Stones = Map[Long, Long]

  val input = "125 17".split(" ").map(_.toLong -> 1L).toMap
  //val input = "2 77706 5847 9258441 0 741 883933 12".split(" ").map(_.toLong -> 1L).toMap

  def blink(n: Long): Stones = n match {
    case 0 => Map(1L -> 1L)
    case x if x.toString.length % 2 == 0 =>
      val s = x.toString
      val l = s.length
      val a = s.substring(0, l / 2).toLong
      val b = s.substring(l / 2).toLong
      Map(a -> 1L, b -> 1L)
    case o => Map(n * 2024 -> 1L)
  }

  def combine(map1: Stones, map2: Stones): Stones =
    // merge together 2 maps with frequencies
    (map1.keySet ++ map2.keySet).map((k: Long) => (k, map1.getOrElse(k, 0L) + map2.getOrElse(k, 0L))).toMap

  def change(stones: Stones): Stones =
    stones.map((stone, freq) => blink(stone).map((stone2, freq2) => (stone2, freq2 + freq))).reduceLeft(combine)

  val res = (0 until 1).foldLeft(input)((a, _) => change(a))
  println(res.values.sum) // 190865

