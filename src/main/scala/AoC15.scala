object AoC15 {
  val input = "9,6,0,10,18,2,1".split(",").map(_.toInt).toSeq

  val N1 = 2020
  val N2 = 30000000

  val tests = Seq (
    (Seq(0, 3, 6), 436),
    (Seq(1, 3, 2), 1),
    (Seq(2, 1, 3), 10),
    (Seq(1, 2, 3), 27),
    (Seq(2, 3, 1), 78),
    (Seq(3, 2, 1), 438),
    (Seq(3, 1, 2), 1836)
  )

  def solveByFold(seed: Seq[Int], iterations: Int) = {
    val res = (seed.length until iterations).foldLeft((seed.zipWithIndex.dropRight(1).toMap, seed.last)) {
      case ((map, last), turn) =>
        val prevToLast = map.get(last)
        val elem = if (prevToLast.nonEmpty)
          turn - prevToLast.get - 1
        else
          0

        (map + (last -> (turn - 1)), elem)
    }

    res._2
  }

  def solveByLoop(seed: Seq[Int], iterations: Int) = {
    val map = scala.collection.mutable.Map.from(seed.zipWithIndex.dropRight(1))
    var last = seed.last
    var turn = seed.length

    while (turn < iterations) {
      val prevToLast = map.get(last)
      val elem = if (prevToLast.nonEmpty)
        turn - prevToLast.get - 1
      else
        0

      map.addOne(last -> (turn - 1))
      last = elem
      turn += 1
    }

    last
  }

  def main(args: Array[String]): Unit = {

    assert(tests.forall { case (seed, expected) => expected == solveByFold(seed, N1) })
    assert(tests.forall { case (seed, expected) => expected == solveByLoop(seed, N1) })

    val ts1 = System.currentTimeMillis()
    val res1 = solveByLoop(input, N1)
    println(s"Problem 1. Result: $res1, computation time: ${System.currentTimeMillis() - ts1} ms")

    val ts2 = System.currentTimeMillis()
    val res2 = solveByLoop(input, N2)
    println(s"Problem 2. Result: $res2, computation time: ${System.currentTimeMillis() - ts2} ms")
  }
}
