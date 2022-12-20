package aoc.aoc2022

import aoc.Aoc

object Day20 extends aoc.Aoc("aoc2022/input20.txt", identity):

  val file = input.map(_.toLong)
  val n    = file.length
  val mod  = n - 1
  println(s"numbers in file: $n")
  println(s"distinct in file: ${file.distinct.length}")
  // NOT DISTINCT NUMBERS 🙈

  def decrypt(key: Long, times: Int): List[Long] =
    // mutable initial state, since the numbers are not unique, map it to an index and search by unique index
    val init = file.map(_ * key).zipWithIndex.toBuffer
    (0 until times).map { _ =>
      file.indices
        .foldLeft(init) { (state, ix) =>
          val fromIx = state.indexWhere(_._2 == ix)
          val item   = state(fromIx)
          val num    = item._1
          val pos    = fromIx + num
          val toIx   = if pos > 0 then pos % mod else n + (pos % mod) - 1
          // println(s"$num => $fromIx -> $toIx, pos=$pos")
          // println(s"state1=$state")
          state.remove(fromIx)
          state.insert(toIx.toInt, item)
          // println(s"state2=$state")
          state
        }
        .map(_._1)
    }.last.toList

  def calc(decrypted: List[Long]): Long =
    val zeroIx = decrypted.indexOf(0L)
    List(1000, 2000, 3000).map(ix => decrypted((zeroIx + ix) % n)).sum

  val res1 = calc(decrypt(1, 1))
  println(s"res1: $res1") // 872 (3)

  val res2 = calc(decrypt(811589153, 10))
  println(s"res2: $res2") // 5382459262696 (1623178306)
