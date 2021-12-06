package sh.perez.advent21

import scala.io.Source

object Day1 extends Day {
  val day = 1

  def solve(n: Int): Int = {
    val values = inputLines().map(_.toInt).sliding(n).toList
    (values, values.tail).zipped.map((a, b) => if (a.sum < b.sum) 1 else 0).sum
  }

  def solveFirst(): Int = solve(1)

  def solveSecond(): Int = solve(3)
}
