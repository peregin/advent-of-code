package aoc.aoc2025

import scala.collection.mutable.ArrayBuffer

object Day8 extends aoc.Aoc("aoc2025/input8.txt", identity):

  case class Point(x: Long, y: Long, z: Long) {
    def dist2(that: Point): Long = {
      val dx = x - that.x
      val dy = y - that.y
      val dz = z - that.z
      dx * dx + dy * dy + dz * dz
    }
  }

  case class Edge(i: Int, j: Int, dist2: Long)

  val points = input.map { case s"$x,$y,$z" =>
    Point(x.toLong, y.toLong, z.toLong)
  }
  val n              = points.length
  val numConnections = if n > 100 then 1000 else 10

  // build all edges (all pairs of boxes) with squared distances
  val edges = new ArrayBuffer[Edge](n * (n - 1) / 2 max 0)

  for {
    i <- 0 until n
    j <- (i + 1) until n
  } {
    val p = points(i)
    val q = points(j)
    edges += Edge(i, j, p.dist2(q))
  }

  // sort edges by distance ascending
  val sortedEdges = edges.sortBy(_.dist2)

  // track circuits
  val parent: Array[Int] = Array.tabulate(n)(i => i)
  val size: Array[Int]   = Array.fill(n)(1)
  var componentCount     = n

  def find(x: Int): Int = {
    var v = x
    while (parent(v) != v) {
      parent(v) = parent(parent(v)) // path compression
      v = parent(v)
    }
    v
  }

  def union(a: Int, b: Int): Unit = {
    var ra = find(a)
    var rb = find(b)
    if (ra == rb) return // already in same circuit
    // union by size
    if (size(ra) < size(rb)) {
      val tmp = ra
      ra = rb
      rb = tmp
    }
    parent(rb) = ra
    size(ra) += size(rb)
    componentCount -= 1
  }

  // connect the closest `numConnections` pairs
  val limit = math.min(numConnections, sortedEdges.length)
  var idx   = 0
  while (idx < limit) {
    val e = sortedEdges(idx)
    union(e.i, e.j)
    idx += 1
  }

  // count sizes
  val compSizes = scala.collection.mutable.Map[Int, Int]()
  for (i <- 0 until n) {
    val root = find(i)
    compSizes.update(root, compSizes.getOrElse(root, 0) + 1)
  }
  val top3  = compSizes.values.toSeq.sorted(Ordering[Int].reverse).take(3).map(_.toLong)
  val part1 = top3.product
  println(s"part1=$part1") // 40, 121770

  // continue until all in one circuit
  var lastEdge: Option[Edge] = None
  while (componentCount > 1 && idx < sortedEdges.length) {
    val e      = sortedEdges(idx)
    val before = componentCount
    union(e.i, e.j)
    if (before > componentCount && componentCount == 1) {
      lastEdge = Some(e)
    }
    idx += 1
  }

  // part 2: multiply the X coordinates of the last two junction boxes connected
  val part2 = lastEdge.map(e => points(e.i).x * points(e.j).x).getOrElse(0L)
  println(s"part2=$part2") // 25272, 7893123992
