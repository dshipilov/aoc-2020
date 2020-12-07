object AoC7 {
  val dataPath = "data/aoc7.txt"
  val testPath = "data/aoc7.test.txt"

  val myLuggageType = "shiny gold"

  case class LuggageQnt(t: String, qnt: Int)

  type Data = List[(String, Option[Array[LuggageQnt]])]
  type MappedData = Map[String, Option[Array[LuggageQnt]]]

  def loadData(path: String): Data = {
    val strData = scala.io.Source.fromFile(path, "UTF-8").getLines().toList

    val mainRegex = "^(.+) bags contain (.+)\\.$".r
    val listRegex = "^\\s*(\\d+) (.+?) bags?".r

    strData.map {
      str =>
        val Some(mainMatcher) = mainRegex.findFirstMatchIn(str)
        val key = mainMatcher.group(1)
        val bags = mainMatcher.group(2)


        val luggageList = if (listRegex.matches(bags)) {
          Some(
            mainMatcher.group(2).split(",").map {
              s =>
                listRegex.findFirstMatchIn(s.trim).map(m => LuggageQnt(m.group(2), m.group(1).toInt)).get
            }
          )
        } else
          None

        key -> luggageList
    }
  }

  def containers(luggage: String, data: Data): Set[String] = {
    data.filter {
      case (_, Some(v)) =>
        v.map(_.t).contains(luggage)
      case _ => false
    }
    .map(_._1).toSet
  }

  def taskOne(data: Data): Int = {
    var nodes = containers(myLuggageType, data)

    var count = nodes.size
    var visitedNodes = nodes

    while (nodes.nonEmpty) {
      nodes = nodes.map(containers(_, data)).flatten diff visitedNodes

      count += nodes.size
      visitedNodes = visitedNodes union nodes
    }

    count
  }

  def computeBagsTree(data: MappedData, currentRoot: String): Int = {
    data(currentRoot).map(_.map {
      case LuggageQnt(t, qnt) =>
        qnt * (1 + computeBagsTree(data, t))
    }.sum).getOrElse(0)
  }

  def taskTwo(data: Data): Int = {
    computeBagsTree(data.toMap, myLuggageType)
  }

  def main(args: Array[String]): Unit = {
    val path = dataPath
    //val path = testPath
    val data = loadData(path)

    println(s"Number of possible ways to carry $myLuggageType bag: ${taskOne(data)}")
    println(s"$myLuggageType bag must contain ${taskTwo(data)} other bags")
  }
}
