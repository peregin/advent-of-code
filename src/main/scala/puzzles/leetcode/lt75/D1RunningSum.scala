package puzzles.leetcode.lt75

import scala.collection.mutable.ListBuffer

object D1RunningSum extends App {

  def runningSum(nums: Array[Int]): Array[Int] = {
    val res = ListBuffer.from(nums)
    nums.indices.foreach {
      case 0 => res(0) = nums(0)
      case i => res(i) = nums(i) + res(i - 1)
    }
    res.toArray
  }

  println(runningSum(Array(1, 2, 3, 4)).mkString(","))
}
