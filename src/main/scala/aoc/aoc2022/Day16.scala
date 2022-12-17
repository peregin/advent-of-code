package aoc.aoc2022

import aoc.Aoc
import scala.collection.parallel.CollectionConverters._

// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
object Day16 extends aoc.Aoc("aoc2022/input16.txt", identity):

  // traverse all nodes, visit the nexts with the highest flow rate
  case class Valve(id: String, rate: Int, tunnels: List[String])

  val lines = input
    .map(_.replace("valves", "valve")
      .replace("leads", "lead")
      .replace("tunnels", "tunnel"))
    .map {
    case s"Valve $v has flow rate=$rate; tunnel lead to valve $list" =>
      Valve(v, rate.toInt, list.split(',').map(_.trim).toList)
  }

  val valves: Map[String, Valve] = lines.map(v => (v.id, v)).toMap

  // find the shortest path between the given verteces
  def find(start: String, end: String): Int = dijkstra(Set(start), end, Map(start -> 0))

  @annotation.tailrec
  def dijkstra(queue: Set[String], end: String, cost: Map[String, Int]): Int =
    val node = queue.minBy(cost)
    if node == end then cost(end)
    else
      val neighbours = valves(node).tunnels.toSet
      // dist[u] + length(u, v) < dist[v]
      val candidates = neighbours.filter(c =>
        !cost.contains(c) || cost(c) > cost(node) + 1
      )
      val newCosts = candidates.foldLeft(cost) {
        case (cost, c) => cost + (c -> (cost(node) + 1))
      }
      dijkstra(queue - node ++ candidates, end, newCosts)

  // calculate distances from every valve to each other, where each vertex has cost 1 (minute) always
  // [BBGG -> 5]
  val distances = valves.keys.toList.combinations(2).flatMap{pair =>
    val cost = find(pair(0), pair(1))
    // the assumption is that in the tunnel can go back and forth between the adjecent nodes
    List(pair.mkString -> cost, pair.reverse.mkString -> cost)
  }.toMap
  val valvesWithPressure = valves.values.filter(_.rate > 0).map(_.id).toSet

  // traverse all possible options, then take max
  def exploreAll(curr: String, remaining:Set[String], minutesLeft: Int, pressureTotal: Int): Int =
    remaining
      .map(candidate => (candidate, minutesLeft - distances(curr + candidate) - 1))
      .filter((_, newMinutesLeft) => newMinutesLeft > 0)
      .map((candidate, newMinutesLeft) =>
        val pressure = newMinutesLeft * valves(candidate).rate
        exploreAll(candidate, remaining - candidate, newMinutesLeft, pressureTotal + pressure)
      )
      .reduceOption(_ max _).getOrElse(pressureTotal)
  end exploreAll
  val res1 = exploreAll("AA", valvesWithPressure, 30, 0)
  println(s"res1: $res1") // 1775 (1651) ~4627ms

  def exploreAll2(curr1: String, curr2: String, remaining: Set[String], minutesLeft1: Int, minutesLeft2: Int, pressureTotal: Int): Int =
    val branch1 = remaining
      .map(candidate => (candidate, minutesLeft1 - distances(curr1 + candidate) - 1))
      .filter((_, newMinutesLeft) => newMinutesLeft > 0)
      .map((candidate, newMinutesLeft) =>
        val pressure = newMinutesLeft * valves(candidate).rate
        exploreAll2(candidate, curr2, remaining - candidate, newMinutesLeft, minutesLeft2, pressureTotal + pressure)
      )
    val branch2 = remaining
      .map(candidate => (candidate, minutesLeft2 - distances(curr2 + candidate) - 1))
      .filter((_, newMinutesLeft) => newMinutesLeft > 0)
      .map((candidate, newMinutesLeft) =>
        val pressure = newMinutesLeft * valves(candidate).rate
        exploreAll2(curr1, candidate, remaining - candidate, minutesLeft1, newMinutesLeft, pressureTotal + pressure)
      )
    val all = branch1 ++ branch2
    all.reduceOption(_ max _).getOrElse(pressureTotal)
  end exploreAll2
  //val res2 = exploreAll2("AA", "AA", valvesWithPressure, 26, 26, 0)
  //println(s"res1: $res2") // ??? (1707) // never ends...

  // TODO:
  // - cache recursion, branches are repetitive
  // - use parallel map or traverse with fibers

