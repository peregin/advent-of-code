package puzzles.lt75

trait ArrayUtils {

  // comma separated numeric values
  def fromString(s: String): Array[Int] = s.split(',').map(_.trim.toInt)
}
