object AoC13 {
  val dataPath = "data/aoc13.txt"

  val testData = Seq(
    "939",
    "7,13,x,x,59,x,31,19"
  ).toList

  def problem1(input: List[String]): Unit = {
    val time = input.head.toInt
    val busIds = input.drop(1).head.split(",")
      .filterNot(_ == "x").map(_.toInt)

    val busDelays = busIds.map {
      id =>
        (id - time % id, id)
    }.sortBy(_._1)

    val (delay, busID) = busDelays.head

    println(s"Task 1. Bus $busID, delay $delay, bus * delay=${busID*delay}")
  }

  def invmod(a: Long, mod: Long) = {
    def extgcd(a: Long, b: Long): (Long, Long, Long) = {
      if (a == 0)
        (b, 0, 1)
      else {
        val (g, x, y) = extgcd(b % a, a)
        (g, y - (b/a) * x, x)
      }
    }

    val (g, x, _) = extgcd(a, mod)

    assert(g == 1)

    (x % mod + mod) % mod
  }

  // This problem appears to be great illustration for chinese remainder theorem.
  // Each busID is a modulo (prime number), value computed in problem 1 is a `reminder'.
  // First, we need to get M - product of all bus IDs (modulus)
  // The timestamp is computed as sum of all bus "reminders" multiplied by its Mi (M without bus IDs modulo)
  // and multiplied by Mi reverse by current modulo.
  // The result sum should taken by modulo M to get a `timestamp`.
  def problem2(input: List[String]): Unit = {
    val data = input.drop(1).head.split(",")
      .zipWithIndex
      .filterNot(_._1 == "x")
      .map{ case (s, idx) => (s.toLong, idx) }

    val M = data.map(_._1).product

    val timestamp = data.map {
      case (a, idx) =>
        val Mi = M / a
        val inv_Mi = invmod(Mi, a)
        val r = a - idx % a

        r * Mi * inv_Mi
    }.sum % M

    println(s"Data: ${data.mkString(",")}, timestamp: ${timestamp}")
  }

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile(dataPath, "UTF-8").getLines().toList

    problem1(input)
    problem2(input)
  }
}
