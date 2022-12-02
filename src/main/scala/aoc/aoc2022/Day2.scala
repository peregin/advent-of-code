package aoc.aoc2022

import aoc.Aoc

/**
 * A,X rock - 1
 * B,Y paper - 2
 * C,Z scissor - 6
 * 6 for winning, 0 for losing
 *
 * Rock defeats Scissors,
 * Scissors defeats Paper,
 * and Paper defeats Rock
 */
object Day2 extends aoc.Aoc("aoc2022/input2.txt", _.trim.replace(" ", "")):

  val score1 = input.map{
    case "AY" => 8
    case "BZ" => 9
    case "CX" => 7
    case "AZ" => 3
    case "BX" => 1
    case "CY" => 2
    case "AX" => 4
    case "BY" => 5
    case "CZ" => 6
  }.sum
  println(s"score1=$score1")

  val score2 = input.map {
    case "AX" => 3 // scissor
    case "BX" => 1 // rock
    case "CX" => 2 // paper
    case "AY" => 4 // draw
    case "BY" => 5 // draw
    case "CY" => 6 // draw
    case "AZ" => 8 // paper
    case "BZ" => 9
    case "CZ" => 7
  }.sum
  println(s"score2=$score2")



