object AoC16 {
  val dataPath = "data/aoc16.txt"
  case class TicketInfo(rules: Map[String, (Range, Range)], ticket: Seq[Int], nearby: Seq[Seq[Int]])

  val testInput =
    """
      |class: 1-3 or 5-7
      |row: 6-11 or 33-44
      |seat: 13-40 or 45-50
      |
      |your ticket:
      |7,1,14
      |
      |nearby tickets:
      |7,3,47
      |40,4,50
      |55,2,20
      |38,6,12
      |""".stripMargin

  val ruleRegex = "^([\\w\\s]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$".r
  val ticketRegex = "^[\\d,]+$".r

  def loadData(input: List[String]): TicketInfo = {
    val (rules, tickets) = input.foldLeft((Map.empty[String, (Range, Range)], Seq.empty[Seq[Int]])) {
      case ((rules, tickets), str) if ruleRegex.matches(str) =>
        val Some(matcher) = ruleRegex.findFirstMatchIn(str)

        val ruleName = matcher.group(1)
        val range1 = Range.inclusive(matcher.group(2).toInt, matcher.group(3).toInt)
        val range2 = Range.inclusive(matcher.group(4).toInt, matcher.group(5).toInt)

        (rules + (ruleName -> (range1, range2)), tickets)

      case ((rules, tickets), str) if ticketRegex.matches(str) =>
        (rules, tickets :+ str.split(",").map(_.toInt).toSeq)

      case ((rules, tickets), _) => (rules, tickets)
    }

    TicketInfo(rules, tickets.head, tickets.tail)
  }

  def rowsToColumns(tickets: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val nCols = tickets.map(_.length).min
    (0 until nCols).map(offset => tickets.map(_.apply(offset)))
  }

  def problem2(data: TicketInfo): Unit = {
    val allTickets = data.ticket +: data.nearby
    val columns = rowsToColumns(allTickets)

    val columnMatches = columns.zipWithIndex.map {
      case (col, idx) =>
        val fields = data.rules
          .filter { case (_, (r1, r2)) => col.forall(x => r1.contains(x) || r2.contains(x)) }.keySet

        (idx, fields)
    }

    val mappings = scala.collection.mutable.Map.empty[String, Int]

    while (mappings.size < columnMatches.size) {
      val exactMatches = columnMatches
        .map { case (idx, fields) => (fields -- mappings.keySet) -> idx }
        .filter(_._1.size == 1)

      mappings.addAll(exactMatches.map { case (fields, idx) => fields.head -> idx })
    }

    val res = mappings
      .filter { case (field, _) => field.startsWith("departure") }
      .map { case (_, idx) => data.ticket(idx).toLong }
      .product

    println(s"Problem 2. Departure fields product: ${res}")
  }

  def main(args: Array[String]): Unit = {
    val data = loadData(scala.io.Source.fromFile(dataPath, "UTF-8").getLines().toList)

    val ranges = data.rules.values.flatMap(x => Seq(x._1, x._2)).toSeq
    val errorRate = data.nearby.flatMap(ticket => ticket.filter(num => ranges.forall(range => !range.contains(num)))).sum

    println(s"Problem 1. Error rate = $errorRate")

    val validNearbyTickets = data.nearby.filter(ticket => ticket.forall(num => ranges.exists(range => range.contains(num))))

    problem2(data.copy(nearby = validNearbyTickets))
  }
}
