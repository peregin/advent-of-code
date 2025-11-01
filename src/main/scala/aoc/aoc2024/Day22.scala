package aoc.aoc2024

import aoc.Aoc

object Day22 extends Aoc("aoc2024/input22.txt", _.toLong):

  private val mod = 16777216L
  def s1(secret: Long): Long = ((secret * 64) ^ secret) % mod
  def s2(secret: Long): Long = ((secret / 32) ^ secret) % mod
  private def s3(secret: Long): Long = ((secret * 2048) ^ secret) % mod
  def calc(secret: Long): Long = s3(s2(s1(secret)))

  val part1 = input.map(n => (1 to 2000).foldLeft(n)((accu, i) => calc(accu))).sum
  println(s"part1=$part1")
