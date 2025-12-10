package aoc.aoc2025

object Day10 extends aoc.Aoc("aoc2025/input10.txt", identity):
  val part1 = input.map { line =>
    val items = line.split(' ')
    val diagram = items.head.tail.dropRight(1)
    val target = diagram.map(c => if c == '#' then 1 else 0).toArray
    val n = target.length
    val buttons = items.tail.dropRight(1).map(s => s.tail.dropRight(1).split(',').map(_.toInt))

    // brute force all combinations
    val k = buttons.length
    var minPresses = Int.MaxValue
    for (mask <- 0 until (1 << k)) {
      val current = Array.fill(n)(0)
      var presses = 0
      for (i <- 0 until k) {
        if ((mask & (1 << i)) != 0) {
          presses += 1
          for (light <- buttons(i)) {
            current(light) ^= 1
          }
        }
      }
      if (current sameElements target) {
        minPresses = math.min(minPresses, presses)
      }
    }
    minPresses
  }.sum

  println(s"part1=$part1") // 7, 509
