import scala.annotation.tailrec

// https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange
object Day25 extends App {

  val input0 = """5764801
                |17807724""".stripMargin // 14897079

  val input = """9232416
                |14144084""".stripMargin

  val modNumber = 20201227
  val subNumber = 7

  @tailrec
  def findLoopSize(pk: Int, v: Long = 1L, loop: Int = 0, sn: Int = subNumber): Int = {
    if (v == pk) loop
    else findLoopSize(pk, (v * sn) % modNumber, loop + 1)
  }

  def findKey(ls: Int, pk: Int): Long = (0 until ls).foldLeft(1L)((accu, _) => (accu * pk) % modNumber)

  val cardPk = input.split("\n")(0).toInt
  val doorPk = input.split("\n")(1).toInt
  val solution1 = findKey(findLoopSize(cardPk), doorPk)
  println(s"solution1=$solution1")
}
