import scala.io.Source

object AoC2 {
  val dataPath = "data/aoc2.txt"
  val pattern = """^(\d+)-(\d+) (\w): (\w+)""".r

  case class Rule(symbol: Char, first: Int, second: Int, pass: String)

  def parse(str: String): Option[Rule] = {
    pattern.findFirstMatchIn(str)
      .flatMap{ m =>
        val groups: Array[String] = m.subgroups.toArray
        Option(
          Rule(
            groups(2).charAt(0),
            groups(0).toInt,
            groups(1).toInt,
            groups(3)
        ))
      }
  }

  def validate1(rule: Rule): Boolean = {
    val nChars = rule.pass.count(_ == rule.symbol)
    rule.first <= nChars && nChars <= rule.second
  }

  def validate2(rule: Rule): Boolean = {
    import rule._

    val p1 = pass.charAt(first - 1) == symbol
    val p2 = pass.charAt(second - 1) == symbol

    p1 && !p2 || p2 && !p1
  }

  def main(args: Array[String]): Unit = {
    val data = Source.fromFile(dataPath, "UTF-8").getLines().toList

    // record sample: 2-13 k: wkbwczdmrgkklvxpppfx

    val results = for {
      line <- data
      rule <- parse(line)
      valid1 <- Option(validate1(rule))
      valid2 <- Option(validate2(rule))
      res <- {
        Option((valid1, valid2, line, rule))
      }
    } yield res

    //results.filterNot(_._2).foreach(r => println(s"not valid (${r._2}): ${r._3} (${r._4.first}-${r._4.second} ${r._4.symbol})"))
    println(s"Valid passwords (first policy): ${results.count(_._1)} out of ${data.size}")
    println(s"Valid passwords (second policy): ${results.count(_._2)} out of ${data.size}")
  }

}
