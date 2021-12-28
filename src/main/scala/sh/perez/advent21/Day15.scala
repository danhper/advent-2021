package sh.perez.advent21

import scala.collection.mutable.PriorityQueue

object Day15 extends Day {
  val day = 15

  lazy val riskMap = {
    Grid(inputLines().zipWithIndex.flatMap((line, y) => {
      line.zipWithIndex.map((c, x) => (Point2D(x, y), c.asDigit))
    }).toMap)
  }

  lazy val fullMap = {
    val Point2D(maxX, maxY) = riskMap.end
    def computeRisk(value: Int): Int = if (value >= 10) (value % 10) + 1 else value
    Grid(riskMap.points.foldLeft(Map.empty[Point2D, Int]) { case (map, (Point2D(x, y), risk)) =>
      (0 until 5).foldLeft(map) { case (m, dx) => {
        (0 until 5).foldLeft(m) { case (m, dy) => {
          m + (Point2D(x + (maxX + 1) * dx, y + (maxY + 1) * dy) -> computeRisk(risk + dx + dy))
        }}
      }}
    })
  }

  case class Grid(points: Map[Point2D, Int]) {
    val end = Point2D(points.keys.map(_.x).max, points.keys.map(_.y).max)
    def apply(p: Point2D): Int = points(p)

    def distanceToEnd(p: Point2D): Int = (p.x - end.x).abs + (p.y - end.y).abs

    def getNeighbors(p: Point2D): Set[Point2D] = {
      Set((p.x - 1, p.y), (p.x + 1, p.y), (p.x, p.y - 1), (p.x, p.y + 1)).
        map((x, y) => Point2D(x, y)).filter(points.contains)
    }
  }

  case class PointWithPriority(priority: Int, point: Point2D)

  implicit val pointOrdering: Ordering[PointWithPriority] = new Ordering[PointWithPriority] {
    override def compare(a: PointWithPriority, b: PointWithPriority) = b.priority.compare(a.priority)
  }

  def expandRiskMap(grid: Grid): Grid = {
    val start = Point2D(0, 0)
    val queue = new PriorityQueue[PointWithPriority]()
    queue.enqueue(PointWithPriority(0, start))

    def expand(points: Set[Point2D], gScore: Map[Point2D, Int], fScore: Map[Point2D, Int]): Map[Point2D, Int] = {
      if (queue.isEmpty) return gScore
      val node = queue.dequeue()
      if (node.point == grid.end) return gScore
      val (newGScore, newFScore, newPoints) = grid.getNeighbors(node.point)
        .map(p => (p, gScore(node.point) + grid(p)))
        .filter((p, g) => !gScore.contains(p) || g < gScore(p)).foldLeft((gScore, fScore, points)) {
          case ((gs, fs, ps), (p, g)) => {
            val f = g + grid.distanceToEnd(p)
            if (!ps.contains(p)) queue.enqueue(PointWithPriority(f, p))
            (gs + (p -> g), fs + (p -> f), ps + p)
          }
        }
      expand(points - node.point, newGScore, newFScore)
    }
    Grid(expand(Set(start), Map(start -> 0), Map(start -> grid.distanceToEnd(start))))
  }

  def reconstructPath(expandedMap: Grid): List[Point2D] = {
    def reconstruct(point: Point2D, path: List[Point2D]): List[Point2D] = {
      if (point == Point2D(0, 0)) return path
      val newPoint = expandedMap.getNeighbors(point).minBy(v => expandedMap(v))
      reconstruct(newPoint, point :: path)
    }
    reconstruct(expandedMap.end, Nil)
  }

  def solve(grid: Grid): Int = {
    val expandedRiskMap = expandRiskMap(grid)
    val path = reconstructPath(expandedRiskMap)
    path.map(v => grid(v)).sum
  }

  def solveFirst(): Int = solve(riskMap)

  def solveSecond(): Int = solve(fullMap)
}
