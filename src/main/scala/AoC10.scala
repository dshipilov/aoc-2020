import scala.annotation.tailrec

object AoC10 {
  val dataPath = "data/aoc10.txt"

  val testData1 = Array(0, 16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4, 22)

  val testData2 = Array(0,
    28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38,
    39, 11,  1, 32, 25, 35,  8, 17,  7,  9,  4,  2, 34, 10,  3, 52
  )

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile(dataPath, "UTF-8").getLines().map(_.toInt).toSeq

    val data = 0 +: input.sorted :+ input.max + 3

    val dist = (data zip data.drop(1)).map {
      case (v1, v2) => v2 - v1
    }

    val fullChain = dist.forall(_ <= 3)
    val res = dist.count(_ == 1) * dist.count(_ == 3)

    println(s"Task1. Full chain: $fullChain, 1 jolts * 3 jolts = $res")

    @tailrec
    def compute(seq: Seq[Int], factor: Long = 1, variants: Long = 1): Long = {
      seq match {
        case Seq(x, _, _, third, _ @_*) if third <= x + 3 =>
          compute(seq.drop(1), factor + 2, variants)
        case Seq(x, _, second, _ @_*) if second <= x + 3 =>
          compute(seq.drop(1), factor + (1 << math.max(0, factor - 4)), variants)
        case seq if seq.length > 2 =>
          compute(seq.drop(1), 1, variants * factor)
        case _ =>
          variants * factor
      }
    }

    println(s"Task2. Number of possible chains: ${compute(data)}")
  }
}
