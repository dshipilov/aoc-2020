import scala.collection.mutable

object AoC8 {
  val dataPath = "data/aoc8.txt"
  val testPath = "data/aoc8.test.txt"

  val opRegex = "^(nop|acc|jmp) ([+-]\\d+)$".r

  def execute(program: Array[String], tweakOpOffset: Int = -1) = {
    var acc = 0
    var pc = 0
    val visitedSteps = mutable.BitSet.empty

    while(!visitedSteps.contains(pc + 1) && pc < program.size)
    {
      visitedSteps.add(pc + 1)

      val Some(matcher) = opRegex.findFirstMatchIn(program(pc))
      val operand = matcher.group(2).toInt

      matcher.group(1) match {
        case "nop" =>
          if (tweakOpOffset == pc)
            pc += operand
          else
            pc += 1
        case "acc" =>
          acc += operand
          pc += 1
        case "jmp" =>
          if (tweakOpOffset == pc)
            pc += 1
          else
            pc += operand
      }
   }

   val programEnds = pc >= program.size

   (acc, pc, programEnds)
  }

  def main(args: Array[String]): Unit = {
    val path = dataPath
    //val path = testPath
    val program = scala.io.Source.fromFile(path, "UTF-8").getLines().toArray

    val (acc, step, _) = execute(program)

    println(s"Loop detected at step $step (${program(step)}). Accumulator value is $acc")

    def isJumpOrNop(offset: Int) = program(offset).matches("^nop|jmp.*")

    Range(0, program.size).filter(isJumpOrNop).find {
      tweakOffset =>
        val (acc, _, ends) = execute(program, tweakOffset)

        if (ends)
          println(s"Program ends after op replacement at ${tweakOffset} (${program(tweakOffset)}). Accumulator = $acc")

        ends
    }
  }
}
