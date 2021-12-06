package sh.perez.advent21

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

case class Point2D(x: Int, y: Int)
case class Line2D(p1: Point2D, p2: Point2D) {
  def isVertical(): Boolean = p1.x == p2.x
  def isHorizontal(): Boolean = p1.y == p2.y
  def isStraight(): Boolean = isVertical() || isHorizontal()

  def coveredPoints(): Set[Point2D] = {
    if (isVertical())
      (math.min(p1.y, p2.y) to math.max(p1.y, p2.y)).map(y => Point2D(p1.x, y)).toSet
    else if (isHorizontal())
      (math.min(p1.x, p2.x) to math.max(p1.x, p2.x)).map(x => Point2D(x, p1.y)).toSet
    else {
      val (stepX, stepY) = (if (p1.x < p2.x) 1 else -1, if (p1.y < p2.y) 1 else -1)
      (p1.x to p2.x by stepX).zip(p1.y to p2.y by stepY).map((x, y) => Point2D(x, y)).toSet
    }
  }
}


trait Day5Parser extends JavaTokenParsers {
  def point(): Parser[Point2D] = (wholeNumber <~ ",") ~ wholeNumber ^^ { case x ~ y => Point2D(x.toInt, y.toInt) }
  def line(): Parser[Line2D] = (point() <~ "->") ~ point() ^^ { case start ~ end => Line2D(start, end) }
  def input(): Parser[List[Line2D]] = rep(line())
}

object Day5 extends Day with Day5Parser {
  val day = 5

  def computeIntersectionCounts(lines: List[Line2D]): Set[Point2D] = {
    lines.flatMap(_.coveredPoints()).foldLeft(Map.empty[Point2D, Int]) { (acc, point) => {
      acc + (point -> (acc.getOrElse(point, 0) + 1))
    }}.filter(_._2 > 1).keySet
  }

  def solve(p: Line2D => Boolean): Int = {
    val Success(lines, _) = parseAll(input(), inputReader())
    computeIntersectionCounts(lines.filter(p)).size
  }

  def solveFirst(): Int = solve(_.isStraight())

  def solveSecond(): Int = solve(_ => true)
}
