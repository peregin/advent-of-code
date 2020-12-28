import scala.annotation.tailrec
import scala.annotation.threadUnsafe
import scala.util.control.Breaks.breakable

// https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange
object Day25 extends App:

  @threadUnsafe lazy val input0 = """5764801
                |17807724""".stripMargin // 14897079
  @threadUnsafe lazy val input = """9232416
                |14144084""".stripMargin

  val modNumber = 20201227
  val subNumber = 7

  @tailrec
  def findLoopSize(pk: Int, v: Long = 1L, loop: Int = 0, sn: Int = subNumber): Int = 
    if (v == pk) then loop else findLoopSize(pk, (v * sn) % modNumber, loop + 1)
  
  def findKey(ls: Int, pk: Int): Long = (0 until ls).foldLeft(1L)((accu, _) => (accu * pk) % modNumber)

  val cardPk = input0.split("\n")(0).toInt
  val doorPk = input0.split("\n")(1).toInt
  val solution1 = findKey(findLoopSize(cardPk), doorPk)
  println(s"solution1=$solution1")
end Day25
