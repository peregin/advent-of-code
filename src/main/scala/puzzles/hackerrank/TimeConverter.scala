package puzzles.hackerrank

/*
 * https://www.hackerrank.com/challenges/time-conversion/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=24-hour-campaign
Example
12:01:00PM
Return '12:01:00'.
12:01:00AM
Return '00:01:00'.
 */
object TimeConverter extends App {

  def timeConversion(s: String): String = {
    val hour = s.substring(0, 2).toInt
    val core = s.drop(2).dropRight(2)
    val adjusted = if (s.endsWith("PM") && hour < 12) hour + 12 else if (s.endsWith("AM") && hour >= 12) hour - 12 else hour
    f"$adjusted%02.0f$core"
  }

  println(timeConversion("07:05:45PM"))
  println(timeConversion("07:05:45AM"))
  println(timeConversion("12:05:45AM"))
  println(timeConversion("12:45:54PM"))
}
