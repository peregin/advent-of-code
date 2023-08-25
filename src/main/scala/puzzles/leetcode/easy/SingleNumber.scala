package puzzles.leetcode.easy

object SingleNumber extends App:

  def singleNumber(nums: Array[Int]): Int = nums.groupBy(identity).find(_._2.length == 1).get._1

  println(singleNumber(Array(4,1,2,1,2)))
