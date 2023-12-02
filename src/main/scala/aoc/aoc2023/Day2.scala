package aoc.aoc2023

import aoc.Aoc

object Day2 extends aoc.Aoc("aoc2023/input2.txt", identity):

  case class Bag(red: Int = 0, green: Int = 0, blue: Int = 0):
    def addRed(v: Int): Bag = copy(red = red + v)
    def addGreen(v: Int): Bag = copy(green = green + v)
    def addBlue(v: Int): Bag = copy(blue = blue + v)
    def max(that: Bag): Bag = Bag(Math.max(this.red, that.red), Math.max(this.green, that.green), Math.max(this.blue, that.blue))
    def power(): Int = red * green * blue
    def exceeds(red: Int, green: Int, blue: Int): Boolean = this.red > red || this.green > green || this.blue > blue

  // parse the input that looks like
  // e.g. Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  val bags = input.map { line =>
    line.split(':').last.split(';').map(seen =>
      seen.split(',').foldLeft(Bag())((bag, combo) => combo.trim match {
        case s"$d red" => bag.addRed(d.toInt)
        case s"$d green" => bag.addGreen(d.toInt)
        case s"$d blue" => bag.addBlue(d.toInt)
      })
    ).reduce(_ max _)
  }

  val res1 = bags.zipWithIndex.filter(!_._1.exceeds(red = 12, green = 13, blue = 14)).map(_._2 + 1).sum
  println(s"res1: $res1")

  val res2 = bags.map(_.power()).sum
  println(s"res2: $res2")
