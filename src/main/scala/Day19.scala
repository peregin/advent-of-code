/**
  * 0: 4 1 5
  * 1: 2 3 | 3 2
  * 2: 4 4 | 5 5
  * 3: 4 5 | 5 4
  * 4: "a"
  * 5: "b"
  *
  * ababbb
  * bababa
  * abbbab
  * aaabbb
  * aaaabbb
  */
object Day19 extends Aso("input19.txt", identity) {

  val rules    = input.takeWhile(_.nonEmpty)
  val messages = input.drop(rules.size + 1)

  sealed trait Input
  case class Letter(n: Char)                 extends Input
  case class Rules(options: List[List[Int]]) extends Input

  def parse(line: String): (Int, Input) =
    line match {
      case s"""$ix: "$c"""" => (ix.toInt, Letter(c.head))
      case s"$ix: $list"    => (ix.toInt, Rules(list.split('|').map(_.trim).map(_.split(' ').map(_.toInt).toList).toList))
      case any              => sys.error(s"unknown input $any")
    }

  val rulesMap = rules.map(parse).toMap
  println(rulesMap)
  val maxMessage = messages.map(_.size).max

  def regexp(map: Map[Int, Input], msgIx: Int, ruleIx: Int = 0): String =
    if (msgIx >= maxMessage) ""
    else {
      map(ruleIx) match {
        case Letter(c) => c.toString
        case Rules(rules) =>
          rules.map { rule =>
            val res = for (i <- 0 until rule.size) yield regexp(map, msgIx+rule.size, rule(i))
            res.mkString
          }.mkString("(", "|", ")")
      }
    }

  val re1 = regexp(rulesMap, 0)
  println(s"reexp1 = $re1")
  val s1 = messages.count(re1.r.matches)
  println(s"solution1 = $s1")

  val re2 = regexp(rulesMap ++ Map(8 -> Rules(List(List(42), List(42, 8))), 11 -> Rules(List(List(42, 31), List(42, 11, 31)))), 0)
  println(s"regexp2 = $re2")
  val s2 = messages.count(re2.r.matches)
  println(s"solution2 = $s2")
}
