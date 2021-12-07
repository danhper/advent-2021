package sh.perez.advent21

object Day7 extends Day {
  val day = 7

  def computeWeightedCost(value: List[Int], target: Int): Int = {
    value.map(v => {
      val n = math.abs(target - v)
      (n * (n + 1)) / 2
    }).sum
  }

  def computeCost(value: List[Int], target: Int): Int = {
    value.map(v => math.abs(target - v)).sum
  }

  def solve(compute: (List[Int], Int) => Int): Int = {
    val values = inputFirstLine().split(",").map(_.toInt).toList
    Range(values.min, values.max).map(compute(values, _)).min
  }

  def solveFirst(): Int = solve(computeCost)

  def solveSecond(): Int = solve(computeWeightedCost)
}
