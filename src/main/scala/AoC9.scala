object AoC9 {
  val dataPath = "data/aoc9.txt"
  val testPath = "data/aoc9.test.txt"

  def pairs(arr: Array[Long]) = {
    arr.dropRight(1).zipWithIndex.flatMap {
      case (v, idx) =>
        Array.fill(arr.length - idx - 1)(v) zip arr.drop(idx + 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val path = dataPath
    //val path = testPath
    val data = scala.io.Source.fromFile(path, "UTF-8").getLines().map(_.toLong).toArray

    val pre = 25

    val Some((value, idx)) = data.zipWithIndex.drop(pre).find {
      case (v0, idx) =>
        val window = data.slice(idx - pre, idx)

        !pairs(window).exists { case (v1, v2) => v1 + v2 == v0 }
    }

    println(s"Suitable value: data($idx) = $value")

    val subset = data.dropRight(data.length - idx).reverse.dropWhile(_ >= value)

    var start = 0
    var end = start + 1
    var sum = subset(start) + subset(end)

    while (sum != value && end < subset.length) {
      if (sum > value) {
        start += 1
        end = start + 1
      } else {
        end += 1
      }

      sum = subset.slice(start, end).sum
    }

    if (sum == value) {
      val seq = subset.slice(start, end).sorted
      val key = seq.min + seq.max

      println(s"Suitable contingent sequence: ${seq.mkString(",")}, key = ${key}")
    }
  }
}
