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

  def regexp(ruleIx: Int = 0): String =
    rulesMap(ruleIx) match {
      case Letter(c) => c.toString
      case Rules(rules) =>
        rules.map { rule =>
          val res = for (i <- 0 until rule.size) yield regexp(rule(i))
          res.mkString
        }.mkString("(", "|", ")")
    }

  val re = regexp()
  println(s"reexp = $re")
  val s1 = messages.count(re.r.matches)
  println(s"solution1 = $s1")
}
