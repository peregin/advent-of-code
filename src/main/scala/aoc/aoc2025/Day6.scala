package aoc.aoc2025

object Day6 extends aoc.Aoc("aoc2025/input6.txt", identity):

  override protected def shouldTrimInput: Boolean = false

  // get the indices from the operator to identify beginning of each number (might start or not with a space)
  val columns = input.last.zipWithIndex.filter(!_._1.isSpaceChar).map(_._2)
  val cells = input.map{line =>
    val pairs = columns :+ line.length+1
    pairs.zip(pairs.tail).map{case (a, b) =>
      line.substring(a, b-1)}
  }
  println(cells.map(_.mkString("[", ",", "]")).mkString("\n"))


  val worksheet = cells.transpose
  def problems(codes: List[String] => List[Long]): Long = {
    worksheet.map { line =>
      val op = line.last
      val nums = codes(line.dropRight(1))
      op.trim match {
        case "+" => nums.sum
        case "*" => nums.product
      }
    }.sum
  }

  val part1 = problems(_.map(_.trim.toLong))
  println(s"part1=$part1") // 4277556, 3785892992137

  val part2 = problems(_.transpose.map(_.mkString.trim.toLong))
  println(s"part2=$part2") // 3263827, 7669802156452



