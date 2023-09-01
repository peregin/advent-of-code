package puzzles.hackerrank

// https://www.hackerrank.com/challenges/compare-the-triplets/problem?isFullScreen=true
object CompareArrays extends App {

  /*
   * Complete the 'compareTriplets' function below.
   *
   * The function is expected to return an INTEGER_ARRAY.
   * The function accepts following parameters:
   *  1. INTEGER_ARRAY a
   *  2. INTEGER_ARRAY b
   */

  def compareTriplets(a: Array[Int], b: Array[Int]): Array[Int] = {
    val res = Array(0, 0)
    a.zip(b).foldLeft(res){case (accu, (a1, b1)) =>
      if (a1 > b1) Array(accu(0)+1, accu(1))
      else if (b1 > a1) Array(accu(0), accu(1)+1)
      else accu
    }
  }

  println(compareTriplets(Array(17, 28, 30), Array(99, 16, 8)).mkString(","))
}
