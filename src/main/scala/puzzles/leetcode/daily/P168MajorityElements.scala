package puzzles.leetcode.daily

object P168MajorityElements extends App {

  def majorityElement(nums: Array[Int]): Int =
    nums.groupMapReduce(identity)(_ => 1)(_ + _).toList.maxBy(_._2)._1

  println(majorityElement(Array(2, 2, 1, 1, 1, 2, 2)))
}
