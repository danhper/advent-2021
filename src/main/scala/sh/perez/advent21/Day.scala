package sh.perez.advent21

import scala.io.Source
import java.io.InputStreamReader

trait Day {
  val day: Int
  def solveFirst(): Any
  def solveSecond(): Any

  lazy val mainInputFile = s"day${day}.txt"
  lazy val testInputFile = s"day${day}-test.txt"
  lazy val isTest = List("yes", "1", "true").contains(sys.env.getOrElse("ADVENT_TEST", "no").toLowerCase)
  lazy val inputFile = if (isTest) testInputFile else mainInputFile

  def inputLines(name: String = inputFile) = Source.fromResource(name).getLines()
  def inputReader(name: String = inputFile): InputStreamReader = Source.fromResource(name).reader
  def inputFirstLine(name: String = inputFile) = inputLines(name).next()
}

object Day {
  val days: Map[Int, Day] = Map(
    1 -> Day1,
    2 -> Day2,
    3 -> Day3,
    4 -> Day4,
    5 -> Day5,
    6 -> Day6
  )

  def get(n: Int): Option[Day] = days.get(n)
}
