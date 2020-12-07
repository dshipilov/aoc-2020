object AoC6 {
  val dataPath = "data/aoc6.txt"
  val testPath = "data/aoc6.test.txt"

  def loadData(path: String): Array[Array[Int]] = {
    val data = scala.io.Source.fromFile(path, "UTF-8").mkString.split("\n\n")

    data.map {
      _.split("\n").map {
        _.foldLeft(0) {
          case (acc, c) =>
            acc | 1 << c - 'a'
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val path = dataPath
    //val path = testPath
    val answers = loadData(path)

    val res1 = answers.map(_.reduce((acc, v) => acc | v )).map(Integer.bitCount).sum
    val res2 = answers.map(_.reduce((acc, v) => acc & v )).map(Integer.bitCount).sum

    println(s"Total answers, part 1: ${res1}")
    println(s"Total answers, part 2: ${res2}")
  }
}
