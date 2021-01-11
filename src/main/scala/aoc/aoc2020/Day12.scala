package aoc.aoc2020

import aoc.Aoc

import scala.annotation.tailrec

object Day12 extends Aoc("aso2020/input12.txt", identity) {

  // for directions
  // pos 1,0 means E
  // pos 0,1 means N
  // pos -1,0 means W
  // pos 0, -1 means S
  case class Position(x: Int, y: Int) {
    def +(that: Position) = Position(this.x + that.x, this.y + that.y)
    def *(steps: Int)     = Position(this.x * steps, this.y * steps)
    def rotateLeft()      = Position(-y, x)
    @tailrec
    final def rotateLeft(times: Int): Position = if (times <= 0) this else rotateLeft().rotateLeft(times - 1)
    def manhattan(that: Position)              = (this.x - that.x).abs + (this.y - that.y).abs
  }

  sealed trait Action
  case class Forward(amount: Int)     extends Action
  case class Left(angle: Int)         extends Action
  case class Right(angle: Int)        extends Action
  case class Move(position: Position) extends Action

  val actions = input.map { step =>
    val code  = step.head
    val units = step.tail.toInt
    code match {
      case 'R' => Right(units)
      case 'L' => Left(units)
      case 'F' => Forward(units)
      case 'N' => Move(Position(0, units))
      case 'S' => Move(Position(0, -units))
      case 'E' => Move(Position(units, 0))
      case 'W' => Move(Position(-units, 0))
    }
  }

  case class Ship(position: Position, waypoint: Position) {
    def movePosition(coord: Position) = Ship(position + coord, waypoint)
    def moveWaypoint(coord: Position) = Ship(position, waypoint + coord)
    def forward(steps: Int)           = Ship(position + waypoint * steps, waypoint)
    def left(times: Int)              = Ship(position, waypoint.rotateLeft(times))
    def manhattan(position: Position) = this.position.manhattan(position)
  }

  def next(ship: Ship, action: Action, mover: Position => Ship): Ship =
    action match {
      case Move(coord)    => mover(coord)
      case Forward(steps) => ship.forward(steps)
      case Left(angle)    => ship.left(angle / 90)
      case Right(angle)   => ship.left(4 - angle / 90)
    }

  val start     = Position(0, 0)
  val end1      = actions.foldLeft(Ship(start, Position(1, 0)))((ship, action) => next(ship, action, ship.movePosition))
  val solution1 = end1.manhattan(start)
  println(s"solution1 = $solution1")

  val end2      = actions.foldLeft(Ship(start, Position(10, 1)))((ship, action) => next(ship, action, ship.moveWaypoint))
  val solution2 = end2.manhattan(start)
  println(s"solution2 = $solution2")
}
