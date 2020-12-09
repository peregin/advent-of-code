

/**
 * FBFBBFF RLR = 0101100 101
 * 44 * 8 + 5 = 357
 * multiplying with 8 means shifting with 3 bits to the left, so we can treat it as a whole number
 */
object Day5 extends Aso("input5.txt", identity) {

  val seats = input.map(_
    .replace('B', '1').replace('F', '0')
    .replace('R', '1').replace('L', '0'))
    .map(Integer.parseInt(_, 2))

  val solution1 = seats.max
  println(s"solution1=$solution1")

  val sortedSeats = seats.sorted
  // pad the missing seats in back and front to have a single missing element in the list
  val paddedList = (1 until sortedSeats.head) ++ sortedSeats ++ (sortedSeats.last + 1 to 1023)
  // use the plain formula for arithmetic series n * (n + 1) / 2 => 1023 * 1024 / 2 = 523776
  // from here deduct the real sum to find the missing element
  val solution2 = 523776 - paddedList.sum
  println(s"solution2=$solution2")
}
