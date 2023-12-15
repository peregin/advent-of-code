package aoc.aoc2023

import aoc.Aoc

object Day15 extends aoc.Aoc("aoc2023/input15.txt", identity):

  def hash(s: String): Int = s.foldLeft(0)((accu, c) => ((accu + c.toInt) * 17) % 256)

  // val hashes = List("HASH").map(hash)
  val hashes = input.head.split(',').map(hash)
  val res1   = hashes.sum
  println(s"res1: $res1") // 515974

  case class Boxes(boxes: Vector[Vector[(String, Int)]]) {
    def +(keyValue: (String, Int)): Boxes = {
      val key -> value = keyValue
      val boxIx        = hash(key)
      val box          = boxes(boxIx)
      val keyIx        = box.indexWhere(_._1 == key)
      val newBox       = if (keyIx < 0) box :+ keyValue else box.updated(keyIx, keyValue)
      Boxes(boxes.updated(boxIx, newBox))
    }

    def -(key: String): Boxes = {
      val boxIx = hash(key)
      Boxes(boxes.updated(boxIx, boxes(boxIx).filterNot(_._1 == key)))
    }

    def focusingPower: Seq[Int] = for {
      (box, boxIx)               <- boxes.zipWithIndex
      ((_, focalLength), slotIx) <- box.zipWithIndex
    } yield (boxIx + 1) * (slotIx + 1) * focalLength
  }

  def empty: Boxes = Boxes(Vector.fill(256)(Vector.empty))

  def lenses(steps: Seq[String]): Boxes =
    steps.foldLeft(empty) {
      case (accu, s"$key=$value") => accu + (key -> value.toInt)
      case (accu, s"$key-")       => accu - key
      case any                    => sys.error(s"unexpected input $any")
    }

  val res2 = lenses(input.head.split(',')).focusingPower.sum
  println(s"res2: $res2") // 265894
