package sh.perez.advent21

import scala.io.Source

object Day1 extends Day {
  def solve(n: Int): Int = {
    val values = Source.fromResource("input1.txt").getLines().map(_.toInt).sliding(n).toList
    (values, values.tail).zipped.map((a, b) => if (a.sum < b.sum) 1 else 0).sum
  }

  def solveFirst(): Int = solve(1)

  def solveSecond(): Int = solve(3)
}
