package sh.perez.advent21


object Day9 extends Day {
  val day = 9

  lazy val heightMap = {
    inputLines().zipWithIndex.flatMap((line, y) => {
      line.zipWithIndex.map((c, x) => ((x, y), c.asDigit))
    }).toMap
  }

  lazy val lowPoints = heightMap.filter((p, h) => h < getNearbyPoints(p).map(heightMap).min)

  def getNearbyPoints(point: (Int, Int)): List[(Int, Int)] = {
    val (x, y) = point
    val points = List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
    points.filter(heightMap.contains)
  }

  def computeBassin(point: (Int, Int)): Set[(Int, Int)] = {
    def expandBassin(currentPoint: (Int, Int), bassin: Set[(Int, Int)]): Set[(Int, Int)] = {
      val newPoints = getNearbyPoints(currentPoint).filter(p => !bassin.contains(p) && heightMap(p) < 9)
      newPoints.foldLeft(bassin + currentPoint ++ newPoints)((b, p) => expandBassin(p, b))
    }
    expandBassin(point, Set.empty)
  }

  def solveFirst(): Int = lowPoints.map(_._2 + 1).sum

  def solveSecond(): Int = {
    lowPoints.map((p, _) => computeBassin(p).size).toList.sorted.takeRight(3).product
  }
}
