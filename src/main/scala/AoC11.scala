import scala.collection.mutable

object AoC11 {
  val dataPath = "data/aoc11.txt"

  object Seats extends Enumeration {
    type Seat = Value
    val Floor, Free, Occupied = Value
  }
  import Seats._
  type SeatingPlan = Array[Array[Seat]]

  def loadData(data: List[String]) = {
    data.map(str => str.map {
      c => c match {
        case 'L' => Free
        case '#' => Occupied
        case _ => Floor
      }
    }.toArray).toArray
  }

  val testData = loadData(List(
    "L.LL.LL.LL",
    "LLLLLLL.LL",
    "L.L.L..L..",
    "LLLL.LL.LL",
    "L.LL.LL.LL",
    "L.LLLLL.LL",
    "..L.L.....",
    "LLLLLLLLLL",
    "L.LLLLLL.L",
    "L.LLLLL.LL"
  ))

  def toString(plan: SeatingPlan): String = {
    plan.map(row => row.map {
      v => v match {
        case Occupied => '#'
        case Free => 'L'
        case Floor => '.'
    }}.mkString("")).mkString("\n") + "\n"
  }

  def dimensions(plan: SeatingPlan) = (plan.length, if (plan.nonEmpty) plan(0).length else 0)

  def allCoords(plan: SeatingPlan): Seq[(Int, Int)] = {
    val (rows, cols) = dimensions(plan)
    for (r <- Range(0, rows); c <- Range(0, cols))
      yield (r,c)
  }

  def occupiedNbrs1(plan: SeatingPlan, row: Int, col: Int): Int = {
    val (rows, cols) = dimensions(plan)
    Seq(
      (row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
      (row, col - 1), (row, col + 1),
      (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)
    )
    .filter { case (r, c) => r >= 0 && r < rows && c >= 0 && c < cols }
    .count { case (r, c) => plan(r)(c) == Occupied }
  }

  def occupiedNbrs2(plan: SeatingPlan, row: Int, col: Int): Int = {
    val (maxRow, maxCol) = dimensions(plan)

    val directions = Array(
      (-1, -1), (-1, 0), (-1, 1),
      (0, -1), (0, 1),
      (1, -1), (1, 0), (1, 1)
    )

    var toCheck = mutable.Set.from(0 until directions.length)
    var occupied = 0
    var r = 1

    while (toCheck.nonEmpty) {
      val checked = mutable.Set.empty[Int]
      toCheck.foreach {
        idx =>
          val (dr, dc) = directions(idx)
          val rowShift = dr * r + row
          val colShift = dc * r + col

          if (rowShift < 0 || rowShift >= maxRow ||
              colShift < 0 || colShift >= maxCol) {
            checked += idx
          } else if (plan(rowShift)(colShift) == Occupied) {
            occupied += 1
            checked += idx
          } else if (plan(rowShift)(colShift) == Free) {
            checked += idx
          }
      }

      toCheck = toCheck -- checked
      r += 1
    }

    occupied
  }

  def step(plan: SeatingPlan,
           neighborFn: (SeatingPlan, Int, Int) => Int,
           tolerance: Int) = {
    val newPlan = plan.map(_.clone())
    var updates = 0

    allCoords(plan).foreach {
      case (row, col) =>
        plan(row)(col) match {
          case Free if neighborFn(plan, row, col) == 0 =>
            newPlan(row)(col) = Occupied
            updates += 1

          case Occupied if neighborFn(plan, row, col) >= tolerance =>
            newPlan(row)(col) = Free
            updates += 1

          case _ =>
        }
    }

    (newPlan, updates)
  }

  def iterate(plan: SeatingPlan,
              neighborFn: (SeatingPlan, Int, Int) => Int,
              tolerance: Int) = {
    var iterations = 0
    var (currentPlan, updates) = step(plan, neighborFn, tolerance)

    while (updates > 0) {
      val (newPlan, newUpdates) = step(currentPlan, neighborFn, tolerance)

      currentPlan = newPlan

      updates = newUpdates
      iterations += 1
    }

    (currentPlan, iterations)
  }

  def solve(plan: SeatingPlan,
            neighborFn: (SeatingPlan, Int, Int) => Int,
            tolerance: Int) = {
    val (finalPlan, iterations) = iterate(plan, neighborFn, tolerance)

    val occupied = allCoords(finalPlan).count{ case (row, col) => finalPlan(row)(col) == Occupied }
    println(s"Occupied count = $occupied after $iterations iterations")
  }

  def main(args: Array[String]): Unit = {
    val input = loadData(scala.io.Source.fromFile(dataPath, "UTF-8").getLines().toList)

    println("Task 1")
    solve(input, occupiedNbrs1, 4)

    println("Task 2")
    solve(input, occupiedNbrs2, 5)
  }
}
