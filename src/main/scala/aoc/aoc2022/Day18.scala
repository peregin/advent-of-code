package aoc.aoc2022

import aoc.Aoc

object Day18 extends aoc.Aoc("aoc2022/input18.txt", identity):

  case class Cube(x: Int, y: Int, z: Int):
    def +(that: Cube): Cube = this.copy(x = this.x + that.x, y = this.y + that.y, z = this.z + that.z)
    lazy val neighbours = adjacent.map(_ + this)

  val adjacent = Set(
    Cube(-1, 0, 0),
    Cube(1, 0, 0),
    Cube(0, -1, 0),
    Cube(0, 1, 0),
    Cube(0, 0, -1),
    Cube(0, 0, 1),
  )

  val cubes = input.map{case s"$x,$y,$z" => Cube(x.toInt, y.toInt, z.toInt)}
  val cubesSet = cubes.toSet
  println(s"${cubes.size} cubes")

  val touches = cubes.map(c => c.neighbours.count(cubesSet.contains)).sum
  val res1 = cubes.size * 6 - touches
  println(s"res1=$res1") // 3364 (64)

  // detect air cubes
  val leftBottomNear = Cube(cubes.map(_.x).min - 1, cubes.map(_.y).min - 1, cubes.map(_.z).min - 1)
  val rightTopFar = Cube(cubes.map(_.x).max + 1, cubes.map(_.y).max + 1, cubes.map(_.z).max + 1)
  println(s"traverse $leftBottomNear -> $rightTopFar")

  // traverse the empty cubes surrounding the given cubes
  def bfs(c: Cube): Set[Cube] = {
    val queue = collection.mutable.Queue[Cube]()
    val seen = collection.mutable.HashSet[Cube]()
    queue.enqueue(c)
    while (queue.nonEmpty) {
      val v = queue.dequeue()
      val next = v.neighbours
        .filterNot(seen.contains)
        .filterNot(cubesSet.contains)
        .filter(a =>
          a.x >= leftBottomNear.x && a.x <= rightTopFar.x &&
          a.y >= leftBottomNear.y && a.y <= rightTopFar.y &&
          a.z >= leftBottomNear.z && a.z <= rightTopFar.z
        )
      seen ++= next
      next.foreach(queue.enqueue)
    }
    seen.toSet
  }
  val surroundingsSet = bfs(leftBottomNear)
  val res2 = cubes.map(_.neighbours.count(surroundingsSet.contains)).sum
  println(s"res2=$res2") // 2006 (58)

