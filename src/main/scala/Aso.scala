import scala.io.Source

class Aso[T](fileName: String, conv: String => T) extends App {

  val input = Source
    .fromResource(fileName)
    .getLines()
    .map(_.trim)
    .map(conv)
    .toList
}
