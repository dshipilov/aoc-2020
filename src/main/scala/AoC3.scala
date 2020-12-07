import scala.io.Source

object AoC3 {
  val dataPath = "data/aoc3.txt"
  val testPath = "data/aoc3.test.txt"

  def toDebugString(v: Int): String = {
    String.format("%31s",
      Integer.toBinaryString(v))
      .replaceAll(" ", ".")
      .replaceAll("0", ".")
      .replaceAll("1", "#")
      .reverse
  }

  def toDebugString(v: Int, pos: Int): String = {
    val str = toDebugString(v)

    val chr = str.charAt(pos) match {
      case '#' => 'X'
      case '.' => 'O'
      case _ => ' '
    }

    str.substring(0, math.max(0, pos)) + chr + str.substring(pos + 1)
  }

  def loadData(path: String): (Int, Array[Int]) = {
    val data = Source.fromFile(path, "UTF-8").getLines().toList

    val lineSize = data.head.length

    def encodeLine(line: String): Int = {
      line.zipWithIndex.foldLeft(0) {
        case (acc, (ch, idx)) =>

          ch match {
            case '#' => acc | 1 << idx
            case _ => acc
          }
      }
    }

    (lineSize, data.map(encodeLine).toArray)
  }

  def getPos(lineIdx: Int, base: Int, step: Int = 3) = (lineIdx * step) % base

  def computeTrees(data: Array[Int], lineSize: Int, stepRight: Int, stepDown: Int = 1): Int = {
    data.zipWithIndex.foldLeft(0) {
      case (acc, (v, lineIdx)) =>

      if (lineIdx % stepDown == 0) {
        val pos = getPos(lineIdx / stepDown, lineSize, stepRight)
        val matches = (1 << pos & v)

        if (matches > 0)
          acc + 1
        else
          acc
      } else
        acc
    }
  }

  def main(args: Array[String]): Unit = {
    val (lineSize, data) = loadData(dataPath)

    val nTrees = computeTrees(data, lineSize, stepRight = 3, stepDown = 1)

    println(s"Trees: $nTrees")

    val stepConf = Seq(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2)
    )

    val res = stepConf.map(c => (c, computeTrees(data, lineSize, stepRight = c._1, stepDown = c._2)))
    val total = stepConf.map(c => computeTrees(data, lineSize, stepRight = c._1, stepDown = c._2)).product

    println(s"Step configurations: ${res.mkString(",")}, total = $total")
  }
}
