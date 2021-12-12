package aoc.aoc2021

import aoc.Aoc

object Day12 extends Aoc("aoc2021/input12.txt", identity):

  // build all edges, from -> to (many) and reverse path
  val edges = input.map{case s"$from-$to" => (from, to)}
  val allEdges = edges.map(_.swap) ++ edges
  val dag = allEdges.groupMap(_._1)(_._2)
  println(s"edges: $dag")

  def isSmall(cave: String): Boolean = cave.forall(_.isLower)

  // twice only one small cave, the rest only once
  def count(from: String, path: List[String], twiceOk: Boolean): Int = {
    if from == "end" then {
      //println(s"path: ${path.mkString(",")}")
      1
    }
    else {
      val candidates = dag(from).filter{
        // start and end can be visited only once
        case cave if cave == "start" => false
        // small cave can't be visited again
        case cave if !twiceOk && isSmall(cave) => !path.contains(cave)
        // a single small cave can be visited twice
        case cave if twiceOk && isSmall(cave) =>
          val smalls = path.filter(isSmall)
          smalls.size - smalls.distinct.size <= 1
        // large cave is ok to be visited many times
        case large => true
      }
      candidates.map(next =>
        count(next, path :+ next, twiceOk)
      ).sum
    }
  }

  val res1 = count("start", List.empty, twiceOk = false)
  println(s"res1=$res1")

  val res2 = count("start", List("start"), twiceOk = true)
  println(s"res2=$res2")
