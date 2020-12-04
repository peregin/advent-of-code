import scala.io.Source

/**
 * hgt:178cm byr:1987 ecl:blu hcl:#866857 iyr:2017
 * pid:222443921 eyr:2027
 *
 * byr:1964 iyr:2001
 * hgt:180cm eyr:1945
 * hcl:#fffffd pid:305189916 ecl:#ac3c49
 * cid:142
 */
object Day4 extends App {

  val input = Source
    .fromResource("input4.txt")
    .getLines()
    .map(_.trim)
    .toList

  val byr = "^byr:(\\d{4})$".r
  val iyr = "^iyr:(\\d{4})$".r
  val eyr = "^eyr:(\\d{4})$".r
  val hgt1 = "^hgt:(\\d+)cm$".r
  val hgt2 = "^hgt:(\\d+)in$".r
  val hcl = "^hcl:#[0-9a-f]{6}$".r
  val ecl = "^ecl:(amb|blu|brn|gry|grn|hzl|oth)$".r
  val pid = "^pid:\\d{9}$".r

  implicit class PimpString(s: String) {
    val v = s.toInt

    def inRange(min: Int, max: Int): Boolean = v >= min && v <= max
  }

  def validFields(s: String): Boolean = s match {
    case byr(year) => year.inRange(1920, 2002)
    case iyr(year) => year.inRange(2010, 2020)
    case eyr(year) => year.inRange(2020, 2030)
    case hgt1(h) => h.inRange(150, 193)
    case hgt2(h) => h.inRange(59, 76)
    case hcl() | ecl(_) | pid() => true
    case _ => false
  }

  def parse(lines: List[String], fields: Set[String], passports: List[Set[String]]): List[Set[String]] = lines match {
    case Nil => passports :+ fields
    case head :: rest if head.isEmpty => parse(rest, Set.empty, passports :+ fields)
    case head :: rest => parse(rest, head.split(' ').filter(validFields).map(_.split(':').head).toSet ++ fields, passports)
  }

  val passports = parse(input, Set.empty, List.empty)
  val mustHave = Set("ecl", "byr", "eyr", "pid", "iyr", "hgt", "hcl")
  val solution = passports.map(mustHave.subsetOf).count(_ == true)
  println(s"solution=$solution")
}
