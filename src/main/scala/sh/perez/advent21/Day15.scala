package sh.perez.advent21

import scala.collection.immutable.ListSet
import scala.collection.immutable.Queue
import edu.stanford.nlp.util.PriorityQueue
import edu.stanford.nlp.util.BinaryHeapPriorityQueue

object Day15 extends Day {
  type Point = (Int, Int)
  val day = 15

  lazy val riskMap = {
    inputLines().zipWithIndex.flatMap((line, y) => {
      line.zipWithIndex.map((c, x) => ((x, y), c.asDigit))
    }).toMap
  }

  lazy val fullMap = {
    val (maxX, maxY) = getEnd(riskMap)
    def computeRisk(value: Int): Int = if (value >= 10) (value % 10) + 1 else value
    riskMap.foldLeft(Map.empty[Point, Int]) { case (map, ((x, y), risk)) =>
      (0 until 5).foldLeft(map) { case (m, dx) => {
        (0 until 5).foldLeft(m) { case (m, dy) => {
          m + ((x + (maxX + 1) * dx, y + (maxY + 1) * dy) -> computeRisk(risk + dx + dy))
        }}
      }}
    }
  }

  def getEnd(map: Map[Point, Int]): Point = (map.keys.map(_._1).max, map.keys.map(_._2).max)

  def getNeighbors(p: Point, map: Map[Point, Int]): Set[Point] = {
    val (x, y) = p
    Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).filter(map.contains)
  }

  def expandRiskMap(map: Map[Point, Int]): Map[Point, Int] = {
    def expand(queue: PriorityQueue[Point], expanded: Map[Point, Int]): Map[Point, Int] = {
      if (queue.isEmpty()) return expanded
      val point = queue.removeFirst()
      val neighbors = getNeighbors(point, map)
      val newMap = neighbors.foldLeft(expanded) { case (ex, p) => {
        val nv = ex(point) + map(p)
        if (ex.get(p).map(nv < _).getOrElse(true)) {
          queue.changePriority(p, -nv)
          ex + (p -> nv)
        } else ex
      } }
      expand(queue, newMap)
    }
    val priorityQueue = new BinaryHeapPriorityQueue[Point]()
    priorityQueue.add((0, 0), 0)
    expand(priorityQueue, Map.empty[(Int, Int), Int] + ((0, 0) -> 1))
  }

  def reconstructPath(expandedMap: Map[Point, Int]): List[Point] = {
    def reconstruct(point: Point, path: List[Point]): List[Point] = {
      if (point == (0, 0)) return path
      val newPoint = getNeighbors(point, expandedMap).minBy(expandedMap)
      reconstruct(newPoint, point :: path)
    }
    reconstruct(getEnd(expandedMap), Nil)
  }

  def solve(map: Map[Point, Int]): Int = {
    val expandedRiskMap = expandRiskMap(map)
    val path = reconstructPath(expandedRiskMap)
    path.map(map).sum
  }

  def solveFirst(): Int = solve(riskMap)

  def solveSecond(): Int = solve(fullMap)
}
