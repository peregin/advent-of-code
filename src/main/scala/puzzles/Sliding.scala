package puzzles

object Sliding extends App {

  def compute(list: List[Double], period: Int): List[Double] =
    list.sliding(period).map(w => w.sum / period).toList

  println(compute(List(1, 2, 3, 4), 2))
}
