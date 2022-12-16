package aoc.aoc2022

import aoc.Aoc

// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
object Day16 extends aoc.Aoc("aoc2022/input16sample.txt", identity):

  // traverse all nodes, visit the nexts with the highest flow rate
  case class Valve(id: String, rate: Int, tunnels: List[String])

  val lines = input
    .map(_.replace("valves", "valve").replace("leads", "lead").replace("tunnels", "tunnel"))
    .map {
    case s"Valve $v has flow rate=$rate; tunnel lead to valve $list" =>
      Valve(v, rate.toInt, list.split(',').map(_.trim).toList)
  }

  val valves = lines.map(v => (v.id, v)).toMap

  val res1 = valves.mkString("\n")
  println(s"res1: $res1")


