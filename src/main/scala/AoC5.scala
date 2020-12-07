object AoC5 {
  val dataPath = "data/aoc5.txt"

  def seatDecoder(seat: String) = {

    val bitStr = seat
      .replaceAll("[FL]", "0")
      .replaceAll("[BR]", "1")

    Integer.parseInt(bitStr, 2)
  }

  def main(args: Array[String]): Unit = {
    val data = scala.io.Source.fromFile(dataPath, "UTF-8").getLines().toList

    val sortedSeats = data.map(seatDecoder).sorted

    println(s"Max seatID: ${sortedSeats.last}")

    val Some((before, after)) = (sortedSeats zip sortedSeats.drop(1)).find {
      case (prev, next) => next - prev > 1
    }

    println(s"Your seat is ${(before + after)/2}")
  }
}
