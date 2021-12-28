package sh.perez.advent21

object Day25 extends Day {
  val day = 25

  case class Grid(points: Map[Point2D, Char]) {
    val width = points.keys.map(_._1).max + 1
    val height = points.keys.map(_._2).max + 1

    def moveHerd(points: Map[Point2D, Char], char: Char, computeNext: (Point2D) => Point2D): Map[Point2D, Char] = {
      points.foldLeft(points) { case (acc, (point, v)) => {
        val nextPoint = computeNext(point)
        if (v == char && points.getOrElse(nextPoint, '.') == '.')
          acc + (point -> '.') + (nextPoint -> v)
        else acc + (point -> acc.getOrElse(point, v))
      } }
    }

    def advance(): Grid = {
      val newPoints = moveHerd(points, '>', p => Point2D(x = (p.x + 1) % width, y = p.y))
      Grid(moveHerd(newPoints, 'v', p => Point2D(x = p.x, y = (p.y + 1) % height)))
    }

    override def toString(): String = {
      (0 until height).map(y => {
        (0 until width).map(x => points.getOrElse(Point2D(x, y), '.')).mkString
      }).mkString("\n")
    }
  }

  lazy val grid = {
    Grid(inputLines().zipWithIndex.foldLeft(Map.empty[Point2D, Char]) { case (m, (line, y)) => {
      line.zipWithIndex.foldLeft(m) { case (m, (c, x)) => m + (Point2D(x, y) -> c) }
    } })
  }

  def advanceUntilDone(grid: Grid): Int = {
    def run(grid: Grid, n: Int): Int = {
      val next = grid.advance()
      if (next == grid) n else run(next, n + 1)
    }
    run(grid, 1)
  }

  def solveFirst(): Int = advanceUntilDone(grid)

  def solveSecond(): Int = 0
}
