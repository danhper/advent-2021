package sh.perez.advent21

object Day8 extends Day {
  type RawNumber = Set[Char]
  case class Entry(signals: List[RawNumber], output: List[RawNumber])

  val day = 8

  lazy val input: List[Entry] = {
    inputLines().map(line => {
      val List(a, b) = line.split("\\|").map(v => v.trim.split(" ").toList.map(_.toSet)).toList
      Entry(a, b)
    }).toList
  }

  def simpleParse(rawNumber: RawNumber): Option[Int] = {
    rawNumber.size match {
      case 2 => Some(1)
      case 3 => Some(7)
      case 4 => Some(4)
      case 7 => Some(8)
      case _ => None
    }
  }

  def makeMapping(entry: Entry): Map[RawNumber, Int] = {
    val allNumbers = entry.signals.toSet
    val mapping = allNumbers.map(n => (simpleParse(n), n)).filter(_._1.isDefined)
                            .map((a, b) => (a.get, b)).toMap
    val conditions: List[(Int, (RawNumber => Boolean))] = List(
      (6, (n => n.size == 6 && (mapping(1) -- n).size == 1)),
      (0, (n => n.size == 6 && (mapping(4) -- n).size == 1)),
      (9, (n => n.size == 6)),
      (3, (n => n.size == 5 && (mapping(1) -- n).isEmpty)),
      (2, (n => n.size == 5 && (mapping(4) -- n).size == 2)),
      (5, (n => n.size == 5)),
    )

    conditions.foldLeft((allNumbers -- mapping.values, mapping)) {
      case ((remaining, mapping), (number, condition)) =>
        val value = remaining.find(condition).get
        (remaining - value, mapping + (number -> value))
    }._2.toList.map((a, b) => (b, a)).toMap
  }

  def getOutput(entry: Entry) = {
    val mapping = makeMapping(entry)
    entry.output.map(mapping).mkString.toInt
  }

  def solveFirst(): Int = input.map(_.output.map(simpleParse).count(_.isDefined)).sum

  def solveSecond(): Int = input.map(getOutput).sum
}
