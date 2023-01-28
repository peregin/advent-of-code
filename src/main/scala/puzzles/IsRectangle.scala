package puzzles

object IsRectangle extends App {

  def solve(A: Int, B: Int, C: Int, D: Int): Int = {
    val ok = List(A, B, C, D).groupBy(identity).values.map(_.size).forall(_ >= 2)
    if (ok) 1 else 0
  }
}
