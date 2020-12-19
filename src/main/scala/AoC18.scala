import scala.collection.mutable.Stack

object AoC18 {

  def parseToRPN(expr: String, precedence: Map[Char, Int]): Stack[Char] = {
    val ops = Stack.empty[Char]
    val rpn = Stack.empty[Char]

    expr.replaceAll("\\s+", "").foreach {
      case c if c.isDigit =>
        rpn.push(c)
      case c if c == '(' =>
        ops.push(c)
      case c if c == ')' =>
        while (ops.head != '(')
          rpn.push(ops.pop)
        ops.pop() // drop '('

      case c if precedence.keySet.contains(c) =>
        if (ops.nonEmpty && (precedence(c) > precedence(ops.head)))
          ops.push(c)
        else {
          while(ops.nonEmpty && (precedence(c) <= precedence(ops.head)))
            rpn.push(ops.pop())

          ops.push(c)
        }

      case _ => // skip
    }

    while(ops.nonEmpty)
      rpn.push(ops.pop())

    rpn.reverse
  }

  def compute(expr: String, precedence: Map[Char, Int]): Long = {
    val rpn = parseToRPN(expr, precedence)
    val operands = Stack.empty[Long]

    while (rpn.nonEmpty) {
      val op = rpn.pop
      op match {
        case '+' =>
          operands.push(operands.pop() + operands.pop())
        case '*' =>
          operands.push(operands.pop() * operands.pop())
        case _ =>
          operands.push(op - '0')
      }
    }

    operands.pop()
  }

  def main(args: Array[String]): Unit = {
    val testExpr = Seq(
      ("1 + (2*3) +(4 * (5 + 6) )", 51),
      ("1 + 2 * 3 + 4 * 5 + 6", 71),
      ("2 * 3 + (4 * 5)", 26),
      ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
      ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240),
      ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632)
    )

    val prec1 = Map('+' -> 1, '*' -> 1, '(' -> 0, ')' -> 0)
    assert(testExpr.forall {
      case (expr, expected) =>
        expected == compute(expr, prec1)
    })

    val exprs = scala.io.Source.fromFile("data/aoc18.txt", "UTF-8").getLines().toList

    val sum1 = exprs.map(e => compute(e, prec1)).sum
    println(s"Sum 1: ${sum1}")

    val prec2 = Map('+' -> 2, '*' -> 1, '(' -> 0, ')' -> 0)
    val sum2 = exprs.map(e => compute(e, prec2)).sum
    println(s"Sum 2: ${sum2}")
  }
}

