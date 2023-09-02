package puzzles.hackerrank

// https://www.hackerrank.com/challenges/birthday-cake-candles/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=24-hour-campaign
object BDayCandles extends App {

  def birthdayCakeCandles(candles: Array[Int]): Int = {
    // Write your code here
    candles.groupMapReduce(identity)(_ => 1)(_ + _).maxBy(_._1)._2
  }

  println(birthdayCakeCandles(Array(3, 2, 1, 3)))
}
