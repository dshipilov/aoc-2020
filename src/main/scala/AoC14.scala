import scala.collection.mutable

object AoC14 {
  val dataPath = "data/aoc14.txt"

  val testData = List(
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
    "mem[8] = 11",
    "mem[7] = 101",
    "mem[8] = 0"
  )

  val maskRegex = "^mask\\s+=\\s+([X01]+)$".r
  val storeRegex = "^mem\\[(\\d+)\\]\\s+=\\s+(\\d+)$".r

  def parseMask(maskStr: String) = {
    val allBitsMask = (1L << maskStr.length) - 1
    maskStr.zipWithIndex.foldLeft((0L, allBitsMask, allBitsMask)) {
      case ((orMask, andMask, xMask), (ch, idx)) =>
        val bit = 1L << (maskStr.length - idx - 1)
        ch match {
          case 'X' =>
            (orMask, andMask, xMask & (~bit))
          case '0' =>
            (orMask, andMask & (~bit), xMask)
          case '1' =>
            (orMask | bit, andMask, xMask)
        }
    }
  }
  def parseMaskLine(str: String) = {
    val Some(matcher) = maskRegex.findFirstMatchIn(str)
    parseMask(matcher.group(1))
  }

  def problem1(input: List[String]): Unit = {
    val (mem, _, _) = input.foldLeft((Map.empty[Long, Long], 0L, 0L)) {
      case ((mem, _, _), str) if maskRegex.matches(str) =>
        val Some(matcher) = maskRegex.findFirstMatchIn(str)
        val (orMask, andMask, _) = parseMask(matcher.group(1))
        (mem, orMask, andMask)

      case ((mem, orMask, andMask), str) if storeRegex.matches(str) =>
        val Some(matcher) = storeRegex.findFirstMatchIn(str)
        val addr = matcher.group(1).toLong
        val value = matcher.group(2).toLong
        (mem + (addr -> ((value & andMask) | orMask)), orMask, andMask)
    }

    val sum = mem.values.sum
    println(s"Problem 1: sum of memory values = ${sum}")
  }

  def floatingPositions(mask: String): List[Int] = {
    mask.zipWithIndex.filter(_._1 == 'X').map(mask.length - 1 - _._2).toList
  }

  def maskToAddrSet(addr: Long, mask: String): Set[Long] = {
    val fpos = floatingPositions(mask)
    val (orMask, _, xMask) = parseMask(mask)
    val baseAddr = (addr & xMask) | orMask
    val numAddresses = 1 << fpos.length

    (0 to numAddresses).map {
      n =>
        fpos.zipWithIndex.foldLeft(baseAddr) {
          case (acc, (fidx, idx)) =>
            val bit = (n >> idx) & 1L

            if (bit > 0)
              acc | (bit << fidx)
            else
              acc
        }
    }.toSet
  }

  def problem2(input: List[String]): Unit = {
    var currentMask = ""
    var sum = 0L
    val usedAddrs = mutable.Set.empty[Long]

    input.flatMap {
      str =>
        val maskOp = maskRegex.findFirstMatchIn(str)
        val memOp = storeRegex.findFirstMatchIn(str)
        if (maskOp.nonEmpty) {
          currentMask = maskOp.get.group(1)
          None
        } else if (memOp.nonEmpty) {
          Some((memOp.get.group(1).toLong, memOp.get.group(2).toLong, currentMask))
        } else
          None
    }
    .reverse
    .foreach {
      case (addr, value, mask) =>
        val matchingAddrs = maskToAddrSet(addr, mask)
        val newAddrs = matchingAddrs -- usedAddrs

        sum += value * newAddrs.size

        usedAddrs.addAll(matchingAddrs)
    }

    println(s"Problem 2: sum of memory values = ${sum}")
  }

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile(dataPath, "UTF-8").getLines().toList

    problem1(input)
    problem2(input)
  }
}
