
import scala.annotation.tailrec

object Day15 extends App {

  @tailrec
  def next(turn: Int, last: Int, spoken: Map[Int, Int], number: Int): Int =
    if (turn + 1 == number) last
    else next(turn + 1, turn - spoken.get(last).getOrElse(turn), spoken + (last -> turn), number)

  def findTurn(input: List[Int], number: Int): Int = next(input.size - 1, input.last, input.zipWithIndex.toMap, number)

  val solution0 = findTurn(List(0, 3, 6), 2020)
  println(s"solution0=$solution0")
  val solution1 = findTurn(List(7,14,0,17,11,1,2), 2020)
  println(s"solution1=$solution1")
  val solution2 = findTurn(List(7,14,0,17,11,1,2), 30000000)
  println(s"solution2=$solution2")
}
