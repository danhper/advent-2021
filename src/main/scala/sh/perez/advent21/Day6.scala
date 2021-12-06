package sh.perez.advent21

import scala.io.Source

object State {
  def fromList(elems: List[Int]): State = {
    State(elems.foldLeft(Map.empty[Int, Long]) { case (acc, elem) =>
      acc + (elem -> (acc.getOrElse(elem, 0L) + 1L))
    })
  }
}

class State(val elements: Map[Int, Long]) {
  def advance(): State = {
    State(elements.foldLeft(Map.empty[Int, Long].withDefaultValue(0L)) {
      case (acc, (0, v)) => acc + (6 -> (acc(6) + v)) + (8 -> v)
      case (acc, (k, v)) => acc + ((k - 1) -> (acc(k - 1) + v))
    })
  }

  def fishesCount: Long = elements.values.sum
}

object Day6 extends Day {
  val day = 6

  def solve(n: Int): Long = {
    val values = inputFirstLine().split(",").map(_.toInt).toList
    val state = State.fromList(values)
    (0 until n).foldLeft(state)((acc, _) => acc.advance()).fishesCount
  }

  def solveFirst(): Long = solve(80)

  def solveSecond(): Long = solve(256)
}
