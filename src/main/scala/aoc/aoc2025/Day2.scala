package aoc.aoc2025

object Day2 extends aoc.Aoc("aoc2025/input2.txt", identity):

  val str = input.head
  val ranges = str.split(',').map { r =>
    val Array(start, end) = r.split('-').map(_.toLong)
    (start, end)
  }

  def isInvalidPart1(id: Long): Boolean = {
    val s = id.toString
    s.length % 2 == 0 && {
      val half = s.length / 2
      s.substring(0, half) == s.substring(half)
    }
  }

  def isInvalidPart2(id: Long): Boolean = {
    val s = id.toString
    val len = s.length
    (1 to len / 2).exists { k =>
      if (len % k == 0) {
        val reps = len / k
        val segment = s.substring(0, k)
        reps >= 2 && (0 until reps).forall(i => s.substring(i * k, (i + 1) * k) == segment)
      } else false
    }
  }

  val invalidIdsPart1 = ranges.flatMap { case (start, end) =>
    (start to end).filter(isInvalidPart1)
  }

  val invalidIdsPart2 = ranges.flatMap { case (start, end) =>
    (start to end).filter(isInvalidPart2)
  }

  val part1 = invalidIdsPart1.sum
  val part2 = invalidIdsPart2.sum
  println(s"part1=$part1")
  println(s"part2=$part2")
