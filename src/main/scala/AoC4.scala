import scala.io.Source

object AoC4 {
  val dataPath = "data/aoc4.txt"
  val testPath = "data/aoc4.test.txt"

  val requiredKeys = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")//, "cid")

  def loadData(path: String): Seq[Map[String, String]] = {
    val data = Source.fromFile(path, "UTF-8").mkString.split("\n\n")

    data.map {
      _.split("\\s+")
        .map(_.split(":"))
        .map(a => a(0)->a(1))
        .toMap
    }
  }

  def validate1(passport: Map[String, String]): Boolean = {
    passport.keySet.intersect(requiredKeys).equals(requiredKeys)
  }

  def validate2(passport: Map[String, String]): Boolean = {
    val allKeysPresent = validate1(passport)
    if (allKeysPresent) {
      val byr = passport("byr")
      val iyr = passport("iyr")
      val eyr = passport("eyr")
      val hgt = passport("hgt")
      val hcl = passport("hcl")
      val ecl = passport("ecl")
      val pid = passport("pid")

      val d4r = "^\\d{4}$".r

      val hgtValid = "^(\\d{2,3})(cm|in)".r.findFirstMatchIn(hgt) match {
        case Some(matcher) if matcher.group(2) == "cm" =>
          Range.inclusive(150, 193).contains(matcher.group(1).toInt)
        case Some(matcher) if matcher.group(2) == "in" =>
          Range.inclusive(59, 76).contains(matcher.group(1).toInt)
        case _ => false
      }

      d4r.matches(byr) && Range.inclusive(1920, 2002).contains(byr.toInt) &&
      d4r.matches(iyr) && Range.inclusive(2010, 2020).contains(iyr.toInt) &&
      d4r.matches(eyr) && Range.inclusive(2020, 2030).contains(eyr.toInt) &&
      hgtValid &&
      "^#[0-9a-f]{6}$".r.matches(hcl) &&
      "^amb|blu|brn|gry|grn|hzl|oth$".r.matches(ecl) &&
      "^\\d{9}$".r.matches(pid)
    } else
      false
  }

  def main(args: Array[String]): Unit = {
    //val path = "data/aoc4.test.txt"
    val path = dataPath
    val docs = loadData(path)

    println(s"Valid passports: ${docs.count(validate2)} out of ${docs.size}")
  }

}
