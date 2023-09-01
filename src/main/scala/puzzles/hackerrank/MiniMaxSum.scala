package puzzles.hackerrank

// https://www.hackerrank.com/challenges/mini-max-sum/
object MiniMaxSum extends App {

  def miniMaxSum(arr: Array[Int]): Unit = {
    // Write your code here
    val list = arr.map(scala.math.BigDecimal.apply).sorted
    val min = list.slice(0, 4).sum
    val max = list.slice(1, 5).sum
    println(s"$min $max")
  }

  miniMaxSum(Array(1, 3, 5, 7, 9))
}
