package aoc.aoc2024

import aoc.Aoc

object Day9 extends Aoc("aoc2024/input9.txt", identity):

  type Disk     = List[Option[Int]]
  type MiniDisk = List[Int]

  val diskMap = input.head.map(_.asDigit).toList
  println(s"map $diskMap")
  val disk = diskMap
    .grouped(2)
    .zipWithIndex
    .flatMap {
      case (List(file, free), ix) => List.fill(file)(Some(ix)) ++ List.fill(free)(None)
      case (List(file), ix)       => List.fill(file)(Some(ix))
    }
    .toList
  println(s"disk ${disk.map {
      case None     => "."
      case Some(ix) => ix.toString
    }.mkString}")

  def compress(on: Disk, accu: MiniDisk): MiniDisk =
    if (on.isEmpty) accu
    else {
      (on.head, on.last) match {
        case (_, None)        => compress(on.init, accu)
        case (Some(ix), _)    => compress(on.tail, accu :+ ix)
        case (None, Some(ix)) => compress(on.tail.init, accu :+ ix)
      }
    }

  val part1 = compress(disk, List.empty).zipWithIndex.map((d, ix) => d.toLong * ix).sum
  println(s"part1: $part1") // 6337921897505
