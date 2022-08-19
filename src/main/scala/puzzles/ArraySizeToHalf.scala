package puzzles

// https://leetcode.com/problems/reduce-array-size-to-the-half/
object ArraySizeToHalf extends App {

  def minSetSize(arr: Array[Int]): Int = {
    val counts = arr.groupBy(identity).view.mapValues(_.size).toMap
    println(counts)



    val res = arr.distinct.combinations(2)
    println(res.toList.map(_.mkString(",")))
    2
  }

  println(minSetSize(Array(3, 3, 3, 3, 5, 5, 5, 2, 2, 7)))
}
