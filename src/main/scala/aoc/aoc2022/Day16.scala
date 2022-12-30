package aoc.aoc2022

import aoc.Aoc
import scala.collection.parallel.CollectionConverters._

// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
object Day16 extends aoc.Aoc("aoc2022/input16.txt", identity):

  // traverse all nodes, visit the nexts with the highest flow rate
  case class Valve(id: String, rate: Int, tunnels: List[String])

  val lines = input
    .map(
      _.replace("valves", "valve")
        .replace("leads", "lead")
        .replace("tunnels", "tunnel")
    )
    .map { case s"Valve $v has flow rate=$rate; tunnel lead to valve $list" =>
      Valve(v, rate.toInt, list.split(',').map(_.trim).toList)
    }

  val valves: Map[String, Valve] = lines.map(v => (v.id, v)).toMap

  // find the shortest path between the given vertices
  def find(start: String, end: String): Int = dijkstra(Set(start), end, Map(start -> 0))

  @annotation.tailrec
  def dijkstra(queue: Set[String], end: String, cost: Map[String, Int]): Int =
    val node = queue.minBy(cost)
    if node == end then cost(end)
    else
      val neighbours = valves(node).tunnels.toSet
      // dist[u] + length(u, v) < dist[v]
      val candidates = neighbours.filter(c => !cost.contains(c) || cost(c) > cost(node) + 1)
      val newCosts = candidates.foldLeft(cost) { case (cost, c) =>
        cost + (c -> (cost(node) + 1))
      }
      dijkstra(queue - node ++ candidates, end, newCosts)

  // calculate distances from every valve to each other, where each vertex has cost 1 (minute) always
  // [BBGG -> 5]
  val distances = valves.keys.toList
    .combinations(2)
    .flatMap { pair =>
      val cost = find(pair(0), pair(1))
      // the assumption is that in the tunnel can go back and forth between the adjacent nodes
      List(pair.mkString -> cost, pair.reverse.mkString -> cost)
    }
    .toMap
  val valvesWithPressure = valves.values.filter(_.rate > 0).map(_.id).toSet

  // traverse all possible options, then take max
  def exploreAll(curr: String, remaining: Set[String], minutesLeft: Int, pressureTotal: Int): Int =
    remaining
      .map(candidate => (candidate, minutesLeft - distances(curr + candidate) - 1))
      .filter((_, newMinutesLeft) => newMinutesLeft > 0)
      .map((candidate, newMinutesLeft) =>
        val pressure = newMinutesLeft * valves(candidate).rate
        exploreAll(candidate, remaining - candidate, newMinutesLeft, pressureTotal + pressure)
      )
      .reduceOption(_ max _)
      .getOrElse(pressureTotal)
  end exploreAll

  val res1 = exploreAll("AA", valvesWithPressure, 30, 0)
  println(s"res1: $res1") // 1775 (1651) ~4627ms

  // combine part 2 with one, try to reduce many branching options in the recursion, otherwise ot won't finish in a reasonable amount of time
  // generate a partitioning with all the possible options for me and elephant, e.g. for A, B, C:
  // List((Set(C, B, A),Set()), (Set(C, B),Set(A)), (Set(C, A),Set(B)), (Set(C),Set(B, A)), (Set(B, A),Set(C)), (Set(B),Set(C, A)), (Set(A),Set(C, B)), (Set(),Set(C, B, A)))
  // take only half of the list, it is symmetric
  // or generate all combinations of 1, 2, 3... size of vales, and the other set will be the missing valves from the combinations
  def partition(valves: Set[String]): List[(Set[String], Set[String])] =
    if (valves.isEmpty) List((Set.empty[String], Set.empty[String]))
    else
      partition(valves.tail).flatMap { case (me: Set[String], elephant: Set[String]) =>
        List((me + valves.head, elephant), (me, elephant + valves.head))
      }
  println(partition(Set("A", "B", "C")))

  val all     = partition(valvesWithPressure)
  val options = all.take(all.size / 2) // remove symmetric duplicates

  val res2 = options.par.foldLeft(0) { case (accuMax, (me: Set[String], elephant: Set[String])) =>
    val meMax       = exploreAll("AA", me, 26, 0)
    val elephantMax = exploreAll("AA", elephant, 26, 0)
    accuMax max (meMax + elephantMax)
  }

  println(s"res2: $res2") // 2351 (1707) 69211ms
