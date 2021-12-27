package sh.perez.advent21

import scala.collection.mutable

object Day23 extends Day {
  val day = 23

  type Move = ((Point2D, Point2D), Int)

  def parseGrid(lines: Seq[String]): Map[Point2D, Char] = {
    lines.zipWithIndex.foldLeft(Map.empty[Point2D, Char]) { case (m, (line, y)) => {
      line.zipWithIndex.foldLeft(m) { case (m, (c, x)) => m + (Point2D(x, y) -> c) }
    } }
  }

  val addition = "  #D#C#B#A#\n  #D#B#A#C#"

  lazy val lines = inputLines().toList
  lazy val grid = parseGrid(lines)

  lazy val extendedGrid = parseGrid(lines.take(3) ++ addition.split("\n") ++ lines.drop(3))

  val destinations = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)
  val destinationCols = destinations.values.toSet
  val costs = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)

  def computePath(start: Point2D, end: Point2D): List[Point2D] = {
    ((start.y - 1 until 1 by -1).map(y => Point2D(start.x, y)).toList ++
      (start.x until end.x by math.signum(end.x - start.x)).map(x => Point2D(x, 1)) ++
      (1 to end.y).map(y => Point2D(end.x, y)).toList).filterNot(_ == start)
  }

  def getPossibleMoves(grid: Map[Point2D, Char], point: Point2D): List[(Point2D, Int)] = {
    val piece = grid(point)
    val ownCol = destinations(piece)
    val onOwnCol = grid.filter((p, c) => p.x == ownCol && c != '#')

    // if can go to destination, go directly and ignore other moves
    if (point.x != ownCol && onOwnCol.forall((p, c) => c == '.' || c == piece)) {
      val nextAvailable = onOwnCol.filter(_._2 == '.').maxBy(_._1.y)._1.y
      if (nextAvailable > 1) {
        val dest = Point2D(ownCol, nextAvailable)
        val path = computePath(point, dest)
        if (path.forall(p => grid.getOrElse(p, ' ') == '.')) {
          return List((dest, path.size * costs(piece)))
        }
      }
    }

    val toCorridor = if (destinationCols.contains(point.x)) {
      // letter blocking above
      if (grid.exists((p, c) => p.x == point.x && p.y < point.y && c.isLetter)) List.empty
      // already in position
      else if (destinations(piece) == point.x && onOwnCol.forall((p, c) => !c.isLetter || c == piece)) List.empty
      else grid.filter((p, c) => !destinationCols.contains(p.x) && p.y == 1 && c == '.').keys.toList
    } else List.empty


    toCorridor.map(p => (p, computePath(point, p)))
      .filter((p, path) => path.forall(p => grid.getOrElse(p, ' ') == '.'))
      .map((p, path) => (p, path.size * costs(piece)))
      .sortBy(_._2)
  }

  def isGridDone(grid: Map[Point2D, Char]): Boolean = {
    grid.filter(_._2.isLetter).forall((p, c) => p.x == destinations(c) && p.y > 1)
  }

  def doMove(grid: Map[Point2D, Char], move: (Point2D, Point2D)): Map[Point2D, Char] = {
    val (from, to) = move
    grid + (to -> grid(from)) + (from -> grid(to))
  }

  def getAllPossibleMoves(grid: Map[Point2D, Char]): List[Move] = {
    grid.filter(_._2.isLetter).keys.flatMap(p =>
      getPossibleMoves(grid, p).map((dest, cost) => ((p, dest), cost))).toList
  }

  def solveGrid(grid: Map[Point2D, Char]): Option[List[Move]] = {
    val cached = mutable.Map.empty[Map[Point2D, Char], Option[List[Move]]]
    def solve(grid: Map[Point2D, Char]): Option[List[Move]] = {
      if (cached.contains(grid)) cached(grid)
      else if (isGridDone(grid)) Some(List.empty[Move])
      else {
        val possibleMoves = getAllPossibleMoves(grid)
        val result = possibleMoves.foldLeft(Option.empty[List[Move]]) { case (acc, (move, cost)) =>
          val res = solve(doMove(grid, move)).map(r => ((move, cost) :: r))
          List(acc, res).collect { case Some(v) => v }.minByOption(_.map(_._2).sum)
        }
        cached += (grid -> result)
        result
      }
    }
    solve(grid)
  }

  def solve(targetGrid: Map[Point2D, Char]): Int = {
    solveGrid(targetGrid).map(_.map(_._2).sum).get
  }

  def solveFirst(): Int = solve(grid)

  def solveSecond(): Int = solve(extendedGrid)
}
