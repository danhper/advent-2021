package sh.perez.advent21

object Day13 extends Day {
  val day = 13

  case class Fold(value: Int, horizontal: Boolean) {
    def get(point: (Int, Int)): Int = if (horizontal) point._2 else point._1
    def replace(point: (Int, Int), value: Int): (Int, Int) =
      if (horizontal) (point._1, value) else (value, point._2)
  }

  class Grid(val values: Set[(Int, Int)], val dims: (Int, Int)) {
    def fold(f: Fold): Grid = {
      new Grid(values.map(point => {
        val v = f.get(point)
        f.replace(point, if (v < f.value) v else 2 * f.value - v)
      }), f.replace(dims, f.value))
    }

    override def toString(): String = {
      (0 until dims._2).map(y => {
        (0 until dims._1).map(x => if (values.contains((x, y))) "#" else ".").mkString("")
      }).mkString("\n")
    }
  }

  lazy val (grid, folds) = {
    val lines = inputLines().toList
    val grid = lines.takeWhile(!_.isEmpty).foldLeft(Set.empty[(Int, Int)]) { case (acc, line) =>
      val Array(x, y) = line.split(",").map(_.toInt)
      acc + ((x, y))
    }
    val folds = lines.dropWhile(!_.isEmpty).tail.map(line => {
      val Array(at, value) = line.split("=")
      Fold(value.toInt, at.last == 'y')
    })

    (new Grid(grid, (grid.map(_._1).max + 1, grid.map(_._2).max + 1)), folds)
  }

  def solveFirst(): Int = grid.fold(folds(0)).values.size

  def solveSecond(): String = {
    "\n" + folds.foldLeft(grid)((acc, fold) => acc.fold(fold)).toString()
  }
}
