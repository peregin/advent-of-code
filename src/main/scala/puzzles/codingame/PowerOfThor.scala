package puzzles.codingame

import math._
import scala.util._
import scala.io.StdIn._

/**
 * https://www.codingame.com/ide/puzzle/power-of-thor-episode-1
 *
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 * ---
 * Hint: You can use the debug stream to print initialTX and initialTY, if Thor seems not follow your orders.
 **/
object Player extends App {
  // lightX: the X position of the light of power
  // lightY: the Y position of the light of power
  // initialTx: Thor's starting X position
  // initialTy: Thor's starting Y position
  val Array(lightX, lightY, initialTx, initialTy) = (readLine split " ").filter(_ != "").map (_.toInt)

  // game loop
  while(true) {
    val remainingTurns = readLine.toInt // The remaining amount of turns Thor can move. Do not remove this line.

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")


    // A single line providing the move to be made: N NE E SE S SW W or NW
    println("SE")
  }
}
