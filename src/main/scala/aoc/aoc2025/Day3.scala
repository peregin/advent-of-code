package aoc.aoc2025

object Day3 extends aoc.Aoc("aoc2025/input3.txt", identity):

  private def maxBankJoltage(bank: String, take: Int): BigInt =
    bank.map(_.asDigit)
      .foldLeft((List.empty[Int], bank.length - take)) { case ((stack, removeLeft), digit) =>
        @annotation.tailrec
        def removeSmaller(s: List[Int], left: Int): (List[Int], Int) =
          if left > 0 && s.nonEmpty && s.last < digit then removeSmaller(s.init, left - 1)
          else (s, left)
        val (newStack, newRemoveLeft) = removeSmaller(stack, removeLeft)
        (newStack :+ digit, newRemoveLeft)
      }
      ._1.take(take)
      .foldLeft(BigInt(0))(_ * 10 + _)

  val part1: BigInt = input.map(maxBankJoltage(_, 2)).sum
  println(s"part1: $part1") // 357 -> 17452

  val part2: BigInt = input.map(maxBankJoltage(_, 12)).sum
  println(s"part2: $part2") // 3121910778619 -> 173300819005913
