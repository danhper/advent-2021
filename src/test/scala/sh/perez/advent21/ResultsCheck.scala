package sh.perez.advent21

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source

class AdventOfCodeTest extends AnyFunSuite {
  def getResults(day: Int): (String, String) = {
    val toDrop = "challenge n: ".length
    val List(a, b) = Source.fromResource(s"day$day-result.txt").getLines().map(_.drop(toDrop)).toList
    (a, b)
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
