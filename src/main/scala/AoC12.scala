object AoC12 {
  val dataPath = "data/aoc12.txt"

  val testData = Seq(
    "F10",
    "N3",
    "F7",
    "R90",
    "F11"
  )

  val cmdRegex = "([ESWNFLR])(\\d++)".r

  def routing1(input: List[String]) = {
    val dirs = Array( (0, 1), (1, 1), (0, -1), (1, -1) )

    input.foldLeft(Array(0, 0), 0) {
      case ((c, d), str) =>
        val Some(m) = cmdRegex.findFirstMatchIn(str)
        (m.group(1), m.group(2).toInt) match {
          case ("E", dist) =>
            c(0) += dist
            (c, d)
          case ("S", dist) =>
            c(1) += dist
            (c, d)
          case ("W", dist) =>
            c(0) -= dist
            (c, d)
          case ("N", dist) =>
            c(1) -= dist
            (c, d)
          case ("F", dist) =>
            val (axis, dir) = dirs(d)
            c(axis) += dir * dist
            (c, d)
          case ("R", 90) |
               ("L", 270) =>
            (c, (d + 1) % dirs.length)
          case ("L", 90) |
               ("R", 270) =>
            (c, if (d > 0) d - 1 else dirs.length - 1)
          case ("L", 180) |
               ("R", 180) =>
            (c, (d + 2) % dirs.length)
        }
    }
  }

  def routing2(input: Seq[String]) = {
    input.foldLeft(Array(0, 0), (10, -1)) {
      case ((c, (wpX, wpY)), str) =>
        val Some(m) = cmdRegex.findFirstMatchIn(str)
        (m.group(1), m.group(2).toInt) match {
          case ("E", dist) =>
            (c, (wpX + dist, wpY))
          case ("S", dist) =>
            (c, (wpX, wpY + dist))
          case ("W", dist) =>
            (c, (wpX - dist, wpY))
          case ("N", dist) =>
            (c, (wpX, wpY - dist))

          case ("F", dist) =>
            c(0) += wpX * dist
            c(1) += wpY * dist
            (c, (wpX, wpY))
          case ("R", 90) |
               ("L", 270) =>
            (c, (wpY * -1, wpX))
          case ("L", 90) |
               ("R", 270) =>
            (c, (wpY, wpX * -1))
          case ("L", 180) |
               ("R", 180) =>
            (c, (wpX * -1, wpY * -1))
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile(dataPath, "UTF-8").getLines().toList

    val (coords, _) = routing1(input)

    val dist = math.abs(coords(0)) + math.abs(coords(1))
    println(s"Task1. ${coords.mkString(",")}: $dist")

    val (coords2, _) = routing2(input)

    val dist2 = math.abs(coords2(0)) + math.abs(coords2(1))
    println(s"Task2. ${coords2.mkString(",")}: $dist2")
  }
}
