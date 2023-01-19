package puzzles.lt75

object D18Median2Arrays extends App {

  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val merged = (nums1 ++ nums2).sorted
    val n      = merged.length
    val half   = n / 2
    if (n % 2 == 0) (merged(half - 1) + merged(half)).toDouble / 2 else merged(half).toDouble
  }

  //println(findMedianSortedArrays(Array(1, 3), Array(6, 9, 10)))
  println(findMedianSortedArrays(Array(1, 2), Array(3, 4)))
}
