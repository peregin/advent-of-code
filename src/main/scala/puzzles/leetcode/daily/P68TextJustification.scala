package puzzles.leetcode.daily

object P68TextJustification extends App:

  def fullJustify(words: Array[String], maxWidth: Int): List[String] =
    def formatLine(words: Array[String], start: Int, end: Int, maxWidth: Int, isLastLine: Boolean): String = {
      val totalWords = end - start + 1
      val totalSpaces = maxWidth - words.slice(start, end + 1).map(_.length).sum
      val extraSpaces = totalWords - 1
      val spacesBetweenWords = if (totalWords == 1 || isLastLine) 1 else totalSpaces / extraSpaces
      val extraSpacesLeft = if (totalWords == 1 || isLastLine) 0 else totalSpaces % extraSpaces

      val sb = new StringBuilder()
      for (i <- start to end) {
        sb.append(words(i))
        if (i < end || isLastLine) {
          val spacesCount = spacesBetweenWords + (if (i - start < extraSpacesLeft) 1 else 0)
          sb.append(" " * spacesCount)
        }
      }
      if (isLastLine || words.length == 1) {
        sb.append(" " * (maxWidth - sb.length))
      }
      sb.toString()
    }

    var result: List[String] = List()
    var start = 0
    var end = 0
    var lineLength = words(0).length

    for (i <- 1 until words.length) {
      val word = words(i)
      if (lineLength + 1 + word.length <= maxWidth) {
        end = i
        lineLength += 1 + word.length
      } else {
        result = result :+ formatLine(words, start, end, maxWidth, isLastLine = false)
        start = i
        end = i
        lineLength = word.length
      }
    }

    result :+ formatLine(words, start, end, maxWidth, isLastLine = true)

  //println(fullJustify(Array("This", "is", "an", "example", "of", "text", "justification."), 16))
  println(fullJustify(Array("What","must","be","acknowledgment","shall","be"), 16))
