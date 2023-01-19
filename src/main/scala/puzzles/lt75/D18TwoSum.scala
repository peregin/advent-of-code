package puzzles.lt75

object D18TwoSum extends App with ArrayUtils:

  def twoSum(nums: Array[Int], target: Int): Array[Int] =
    val nix = nums.zipWithIndex.groupMap(_._1)(_._2)
    val x = nums.find{n =>
      val d = target - n
      val maybe = nix.get(d)
      if (n == d) maybe.exists(_.length > 1) else maybe.nonEmpty
    }.get
    if (target - x == x) nix(x).take(2) else Array(nix(x).head, nix(target-x).head)

  println(twoSum(fromString("3,3"), 6).mkString(","))
  println(twoSum(fromString("3,2,4"), 6).mkString(","))
