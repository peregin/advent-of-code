package puzzles.hackerrank

// https://www.hackerrank.com/challenges/designer-pdf-viewer/problem
object PDFViewer extends App {

  def designerPdfViewer(h: Array[Int], word: String): Int = {
    word.map(_.asDigit - 10).map(ix => h(ix)).max * word.length
  }

  println(s"area=${designerPdfViewer("1 3 1 3 1 4 1 3 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 7".split(' ').map(_.trim.toInt), "abc")}")
}
