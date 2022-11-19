package puzzles.lt75

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object D1PivotIndex extends App {

  def pivotIndex(nums: Array[Int]): Int = {
    @tailrec
    def check(ix: Int, left: Int, right: Int): Int = {
      val n = nums.length
      if (ix >= n) -1
      else {
        val c = nums(ix)
        if (left == right - c) ix
        else check(ix+1, left + c, right - c)
      }
    }
    check(0, 0, nums.sum)
  }

  println(pivotIndex(Array(1,7,3,6,5,6)))
  println(pivotIndex(Array(2,1,-1)))
  println(pivotIndex(Array(-1,-1,-1,1,1,1)))
}
