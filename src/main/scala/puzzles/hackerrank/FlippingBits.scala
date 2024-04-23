package puzzles.hackerrank

// https: //www.hackerrank.com/challenges/flipping-bits
object FlippingBits extends App {

  /*
   * Complete the 'flippingBits' function below.
   *
   * The function is expected to return a LONG_INTEGER.
   * The function accepts LONG_INTEGER n as parameter.
   */
  def flippingBits(n: Long): Long = {
    val b = n.toBinaryString
    val s = "".padTo(32 - b.length, '0') + b
    val flipped = s.map {
      case '0' => '1'
      case '1' => '0'
    }
    // convert binary string to long
    val res = java.lang.Long.parseLong(flipped, 2)
    //println(s)
    //println(flipped)
    res
  }

  println(s"${flippingBits(9)}")
}
