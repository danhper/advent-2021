package sh.perez.advent21

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source

class AdventOfCodeTest extends AnyFunSuite {
  def getResults(day: Int): (String, String) = {
    val toDrop = "challenge n: ".length
    val lines = Source.fromResource(s"day$day-result.txt").getLines().toList
    val indices = List(1, 2).map(i => lines.indexWhere(_.startsWith(s"challenge $i:")))
    val getResult = (s: Int, e: Int) => (lines(s).drop(toDrop) :: lines.slice(s + 1, e)).mkString("\n")
    (getResult(indices(0), indices(1)), getResult(indices(1), lines.length))
  }

  Day.days.toList.sortBy(_._1).foreach { case (number, day) =>
    test(s"Day $number") {
      val (result1, result2) = (day.solveFirst().toString, day.solveSecond().toString)
      val (expected1, expected2) = getResults(number)
      assert(result1 === expected1)
      assert(result2 === expected2)
    }
  }
}
