package sh.perez.advent21

type Point = (Int, Int)

object Day11 extends Day {
  type GridValues = Map[Point, Int]

  val day = 11

  class Grid(val values: GridValues) {
    override def toString() = {
      val maxX = values.keys.map(_._1).max
      val maxY = values.keys.map(_._2).max
      (0 to maxX).map(x => (0 to maxY).map(y => values((y, x))).mkString("")).mkString("\n")
    }

    def getNeighbors(point: Point): Set[Point] = {
      val (x, y) = point
      (-1 to 1)
        .flatMap(dx => (-1 to 1).map(dy => (x + dx, y + dy)))
        .filter(p => p != point && values.contains(p))
        .toSet
    }

    def advance(): (Grid, Set[Point]) = {
      def processPoint(acc: (GridValues, Set[Point]), point: Point): (GridValues, Set[Point]) = {
        val (values, exploded) = acc
        val n = values(point)
        if (n >= 9 && !exploded.contains(point)) {
          val (newValues, newExploded) = (values + (point -> (n + 1)), exploded + point)
          getNeighbors(point).foldLeft((newValues, newExploded))(processPoint)
        } else {
          (values + (point -> (n + 1)), exploded)
        }
      }

      val (newValues, newExploded) = values.keys.foldLeft((values, Set.empty[Point]))(processPoint)
      (new Grid(newValues.mapValues(v => if (v > 9) 0 else v).toMap), newExploded)
    }
  }

  lazy val initialGrid = new Grid(inputLines().zipWithIndex.foldLeft(Map.empty[Point, Int]) {
    case (mapping, (line, y)) => {
      line.zipWithIndex.foldLeft(mapping) { case (m, (char, x)) =>
        m + ((x, y) -> char.asDigit)
      }
    }
  })

  def solveFirst(): Int = {
    (0 until 100).foldLeft((initialGrid, 0)) { case ((grid, sum), _) => {
      val (newGrid, exploded) = grid.advance()
      (newGrid, sum + exploded.size)
    } }._2
  }

  def solveSecond(): Int = {
    def search(grid: Grid, step: Int): Int = {
      if (grid.values.values.forall(_ == 0)) step
      else search(grid.advance()._1, step + 1)
    }
    search(initialGrid, 0)
  }
}
