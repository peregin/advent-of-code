package aoc.aoc2022

import aoc.Aoc
import Day19.Harvest._
import scala.collection.parallel.CollectionConverters._

object Day19 extends aoc.Aoc("aoc2022/input19.txt", identity):

  enum Harvest:
    case Ore, Clay, Obsidian, Geode
  case class Minerals(ore: Int, clay: Int, obsidian: Int, geode: Int):
    def covers(cost: Minerals): Boolean = this.ore >= cost.ore && this.clay >= cost.clay && this.obsidian >= cost.obsidian && this.geode >= cost.geode
    def -(cost: Minerals): Minerals = Minerals(this.ore - cost.ore, this.clay - cost.clay, this.obsidian - cost.obsidian, this.geode - cost.geode)
    def +(cost: Minerals): Minerals = Minerals(this.ore + cost.ore, this.clay + cost.clay, this.obsidian + cost.obsidian, this.geode + cost.geode)

  case class Blueprint(ix: Int, robotCost: Map[Harvest, Minerals])

  val prints = input.map{
    case s"Blueprint $ix: Each ore robot costs $oreOre ore. Each clay robot costs $clayOre ore. Each obsidian robot costs $obsidianOre ore and $obsidianClay clay. Each geode robot costs $geodeOre ore and $geodeObsedian obsidian." =>
      Blueprint(
        ix = ix.toInt,
        robotCost = Map(
          Harvest.Ore -> Minerals(oreOre.toInt, 0, 0, 0),
          Harvest.Clay -> Minerals(clayOre.toInt, 0, 0, 0),
          Harvest.Obsidian -> Minerals(obsidianOre.toInt, obsidianClay.toInt, 0, 0),
          Harvest.Geode -> Minerals(geodeOre.toInt, 0, geodeObsedian.toInt, 0),
        )
      )
  }
  println(prints.mkString("\n", "\n", "\n"))

  case class State(minute: Int, maxMinute: Int, robots: Map[Harvest, Int], inventory: Minerals, bp: Blueprint):
    lazy val robotsHarvest = Minerals(robots(Ore), robots(Clay), robots(Obsidian), robots(Geode))
    lazy val over = minute > maxMinute
    def harvest(): State = copy(minute = minute + 1, inventory = inventory + robotsHarvest)
    def canBuy(on: Harvest): Boolean = inventory.covers(bp.robotCost(on))
    def buy(on: Harvest): State = copy(inventory = inventory - bp.robotCost(on), robots = robots.updatedWith(on)(_.map(_ + 1)))

    @annotation.tailrec
    final def targetBuy(on: Harvest, from: State = this): Option[State] =
      if from.canBuy(on) then Some(from.harvest().buy(on))
      else
        val next = from.harvest()
        if next.over then None
        else from.targetBuy(on, next)
    end targetBuy


    // iterate and buy geode or trigger all 4 branches recursively and take the best
    def iterate(): State =
      //println(this)
      if over then this // end condition
      else if canBuy(Geode) then harvest().buy(Geode).iterate() // optimize of geode
      else
        // shedule what to buy next, try possible options
        val options = List(targetBuy(Ore), targetBuy(Clay), targetBuy(Obsidian), targetBuy(Geode)).flatMap(_.map(_.iterate()))
        // if no solution then only harvest current state and try the next
        options.maxByOption(_.inventory.geode).getOrElse(harvest().iterate())
  end State

  def calculate(on: List[Blueprint], maxMinutes: Int): List[State] = on.par.map{bp =>
    println(s"calculating blueprint[${bp.ix}]...")
    val robots: Map[Harvest, Int] = Map(Ore -> 1, Clay -> 0, Obsidian -> 0, Geode -> 0)
    val inventory = Minerals(0, 0, 0, 0)
    val last = State(1, maxMinutes, robots, inventory, bp).iterate()
    println(s"ready with [${bp.ix}]=${last.inventory.geode}")
    last
  }.toList

  // 16317ms
  val res1 = calculate(prints, 24).map(s => s.bp.ix * s.inventory.geode).sum
  println(s"res1: $res1") // 1703 (33)

  // 679343ms... cache recursion states in iterate() ? DP?
  val res2 = calculate(prints.take(3), 32).map(_.inventory.geode).product
  println(s"res2: $res2")





