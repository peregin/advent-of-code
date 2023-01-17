package puzzles.lt75

// not overlapping intervals, create a new non overlapping list after insertion by merging the overlapping ones
object D16IntervalInsert extends App:

  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    // partition, take the disjoint left and right, merge the intervals between with the new one
    intervals
  }

  val res = insert(Array(Array(1,3), Array(6,9)), Array(2, 5))
  println(s"res=${res.map(_.mkString(",")).mkString("|")}")

