package sh.perez.advent21

import scala.io.Source
import java.io.InputStreamReader

trait Day {
  val day: Int
  def solveFirst(): Any
  def solveSecond(): Any

  lazy val mainInputFile = s"day${day}.txt"
  lazy val testInputFile = s"day${day}-test.txt"
  lazy val isTest =
    List("yes", "1", "true").contains(sys.env.getOrElse("ADVENT_TEST", "no").toLowerCase)
  lazy val inputFile = if (isTest) testInputFile else mainInputFile

  def inputLines(name: String = inputFile) = Source.fromResource(name).getLines()
  def inputReader(name: String = inputFile): InputStreamReader = Source.fromResource(name).reader
  def inputFirstLine(name: String = inputFile) = inputLines(name).next()
}

object Day {
  val days: Map[Int, Day] = List(
    Day1,
    Day2,
    Day3,
    Day4,
    Day5,
    Day6,
    Day7,
    Day8,
    Day9,
    Day10,
    Day11,
    Day12,
    Day13,
    Day14,
    Day15,
    Day16,
    Day17,
    Day18,
    Day20,
  ).map(d => d.day -> d).toMap

  def get(n: Int): Option[Day] = days.get(n)
}
