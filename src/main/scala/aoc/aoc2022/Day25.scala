package aoc.aoc2022

import aoc.Aoc

object Day25 extends aoc.Aoc("aoc2022/input25.txt", identity):

  val snafu = input.map(decode)

  def decode(s: String): Long =
    s.reverse.zipWithIndex.foldLeft(0L) { case (accu, (c, exp)) =>
      val m = math.pow(5, exp).toLong
      val d = c match {
        case '-' => -1
        case '=' => -2
        case o   => o.asDigit
      }
      m * d + accu
    }

  // if ends with 3 or 4, then add 5 * m
  def encode(n: Long): String =
    if n <= 0 then ""
    else
      val m    = n % 5
      val (carry, digit) = m match {
        case 0 => (0, "0")
        case 1 => (0, "1")
        case 2 => (0, "2")
        case 3 => (5, "=")
        case 4 => (5, "-")
      }
      val rest = (n - m + carry) / 5
      encode(rest) + digit

  val all  = snafu.sum
  println(s"sum: $all")
  val res1 = encode(all)
  println(s"res1: $res1") // 36671616971741 = 20=02=120-=-2110-0=1 (4890 = 2=-1=0)