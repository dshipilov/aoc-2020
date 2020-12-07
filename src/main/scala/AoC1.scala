import scala.io.Source

object AoC1 {
  val dataPath = "data/aoc1.txt"
  val sum = 2020

  def by2(sum: Int, data: Array[Int]) = {
    val limitedData = data.takeWhile(_ < sum)

    val results = limitedData.view.map { v => (v, limitedData.find(_ == sum - v)) }.filter(_._2.nonEmpty).take(1)

    results.map { case (v1, Some(v2)) => (v1, v2, v1 * v2) }.headOption
  }

  def by3(sum: Int, data: Array[Int]) = {
    data.map (v => (v, by2(sum - v, data)))
      .filter { case (_, opt) => opt.nonEmpty }
      .map { case (v1, Some((v2, v3, _))) => (v1, v2, v3, v1*v2*v3) }
  }

  def main(args: Array[String]): Unit = {
    val data = Source.fromFile(dataPath, "UTF-8").getLines().map(_.toInt).toArray.sorted

    println(s"Sum $sum by 2: ${by2(sum, data).mkString(",")}")
    println(s"Sum $sum by 3: ${by3(sum, data).mkString(",")}")
  }
}
