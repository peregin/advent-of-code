package aoc.aoc2025

object Day5 extends aoc.Aoc("aoc2025/input5.txt", identity):

  case class Range(start: Long, end: Long) {
    def contains(value: Long): Boolean = value >= start && value <= end
    // merge overlapping ranges or keep it as it is if disjunct
    def merge(that: Range): List[Range] = {
      if (this.end < that.start || that.end < this.start) List(this, that)
      else List(Range(Math.min(this.start, that.start), Math.max(this.end, that.end)))
    }
    def size: Long = end - start + 1
  }

  object Range {
    def apply(line: String): Range = {
      val List(start, end) = line.split("-").map(_.toLong).toList
      Range(start, end)
    }
  }

  val List(rangesText, ingredientsText) = splitByEmptyLine(input)
  val ranges = rangesText.map(Range.apply)
  val ingredients = ingredientsText.map(_.toLong)
  val validIngredients = ingredients.count(i => ranges.exists(_.contains(i)))
  println(s"part1 = $validIngredients") // 862

  val merged = ranges.sortBy(_.start).foldLeft(List[Range]())((acc, r) => acc match {
    case Nil => List(r)
    case h :: t => r.merge(h) ::: t
  })
  val part2 = merged.map(_.size).sum
  println(s"part2 = $part2") // 357907198933892



