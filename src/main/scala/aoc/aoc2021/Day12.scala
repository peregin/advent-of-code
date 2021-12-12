package aoc.aoc2021

import aoc.Aoc

object Day12 extends Aoc("aoc2021/input12.txt", identity):

  // build all edges, from -> to (many) and reverse path
  val edges = input.map{case s"$from-$to" => (from, to)}
  val allEdges = edges.map(_.swap) ++ edges
  val dag = allEdges.groupMap(_._1)(_._2)
  println(s"edges: $dag")

  def isSmall(cave: String): Boolean = cave.forall(_.isLower)

  def count(from: String, visited: Set[String], path: List[(String, String)]): Int = {
    if from == "end" then {
      //println(s"path: ${path.reverse}")
      1
    }
    else {
      val candidates = dag(from).filter{
        // small cave can't be visited again
        case cave if isSmall(cave) => !visited.contains(cave)
        // large case is ok but the same edge can't be visited twice
        case large => !path.contains((from, large))
      }
      candidates.map(next => count(next, visited + from, (from, next) :: path)).sum
    }
  }

  val res1 = count("start", Set.empty, List.empty)
  println(s"res1=$res1")
