object Day20 extends Aso("input20_1.txt", identity) {

  case class Tile(id: Long, lines: List[String]) {
    //rotate to left
    def rotate(): Tile = copy(lines = lines.map(_.toList).transpose.map(_.mkString).reverse)
    def flip(): Tile   = copy(lines = lines.reverse)
    lazy val rotations: List[Tile] =
      List(
        this,
        rotate(),
        rotate().rotate(),
        rotate().rotate().rotate()
      )
    lazy val options = rotations ++ flip().rotations
    lazy val top     = lines.head
    lazy val bottom  = lines.last
    lazy val left    = lines.map(_.head).mkString
    lazy val right   = lines.map(_.last).mkString
    lazy val borders = List(left, right, top, bottom)

    override def toString: String = s"$id:\n${lines.mkString("\n")}"
  }

  case class Grid(tiles: List[Tile]) {
    def valid(): Boolean = {
      val verticalBorders =
        for (i <- 0 until tiles.size - 1 by square)
          yield tiles(i).right.zip(tiles(i + 1).left).forall(b => b._1 == b._2)
      val horizontalBorders = for (i <- 0 until tiles.size - square by square)
        yield tiles(i).bottom.zip(tiles(i + square).top).forall(b => b._1 == b._2)
      verticalBorders.forall(_ == true) && horizontalBorders.forall(_ == true)
    }

    lazy val corners  = List(tiles.head.id, tiles.last.id, tiles(square - 1).id, tiles(tiles.size - square).id)
    lazy val solution = corners.product
  }

  val groups = splitByEmptyLine(input)
  val tiles = groups.map { lines =>
    val id = lines.head.split(' ')(1).stripSuffix(":").toLong
    Tile(id, lines.tail)
  }
  val square = Math.sqrt(tiles.size).toInt
  val opts   = tiles.head.options.size
  val combs  = Math.pow(opts, tiles.length).toLong
  println(s"tiles=${tiles.size}, square=$square, options=$opts, combinations=$combs")

  // possible options
  val grids = (0L until combs).to(LazyList).map { ix =>
    val variation = java.lang.Long.toString(ix, opts).reverse.padTo(tiles.size, '0').reverse
    Grid(tiles.zipWithIndex.map {
      case (tile: Tile, ix: Int) =>
        val rotIx = variation.charAt(ix).asDigit
        tile.options(rotIx)
    })
  }

  val puzzle    = grids.find(_.valid).get

  val solution1 = puzzle.solution
  println(s"solution1=$solution1")
}
