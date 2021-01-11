package aso.aso2020

import aso.Aso

/**
  * dark orange bags contain 3 bright white bags, 4 muted yellow bags.
  * faded blue bags contain no other bags.
  */
object Day7 extends Aso("aso2020/input7.txt", identity) {

  val myShinyGold = "shiny gold"

  case class Node(color: String, count: Int)

  // key = bag color, value is a list of bags
  // ugly parsing, probably could be optimized
  val parents: Map[String, List[Node]] = input.map { line =>
    val items       = line.split("contain")
    val parentColor = items(0).trim.stripSuffix("bags").stripSuffix("bag").trim
    val children = items(1).stripSuffix(".").trim match {
      case "no other bags" => List.empty
      case bags =>
        bags
          .split(',')
          .map(_.stripSuffix("bags").stripSuffix("bag").trim)
          .map { bag =>
            val count = bag.takeWhile(_ != ' ')
            val color = bag.stripPrefix(count).trim
            Node(color, count.toInt)
          }
          .toList
    }
    (parentColor, children)
  }.toMap

  def hasBagWith(color: String, target: String): Boolean = {
    //println(s"looking into $color")
    val children = parents(color)
    val colors   = children.map(_.color)
    colors.exists(_ == target) || colors.exists(hasBagWith(_, target))
  }

  val solution1 = parents.keys.toList.map(hasBagWith(_, myShinyGold)).count(_ == true)
  println(s"solution1=$solution1")

  def countBagsFor(color: String): Int = {
    val children = parents(color)
    children.map { node =>
      //println(s"looking into $node")
      node.count * countBagsFor(node.color) + node.count
    }.sum
  }

  val solution2 = countBagsFor(myShinyGold)
  println(s"solution2=$solution2")
}
