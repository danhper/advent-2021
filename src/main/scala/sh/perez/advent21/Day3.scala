package sh.perez.advent21

import scala.io.Source

object Day3 extends Day {
  type BitCount = Map[Int, Map[Int, Int]]

  def countBits(lines: Iterable[String]): BitCount = {
    val result = Map.empty[Int, Map[Int, Int]].withDefaultValue(Map.empty.withDefaultValue(0))
    lines.foldLeft(result) { case (acc, line) => {
      line.zipWithIndex.foldLeft(acc) { case (m, (c, i)) => {
        m + (i -> (m(i) + (c.asDigit -> (m(i)(c.asDigit) + 1))))
      }}
    }}
  }

  def binToDec(value: Iterable[Int]): Int = value.foldLeft(0) { case (acc, v) => acc * 2 + v }

  def getValue(countedBits: BitCount, mostCommon: Boolean): Int = {
    val bits = countedBits.toList.sortBy(_._1).map { (_, v) =>
      if (mostCommon == v(0) > v(1)) 0 else 1
    }
    binToDec(bits)
  }

  def findValue(lines: Iterable[String], mostCommon: Boolean): Int = {
    def doFind(index: Int, remainingLines: Iterable[String]): String = {
      if (remainingLines.size == 1) return remainingLines.head
      val count = countBits(remainingLines)(index)
      val toKeep = if ((count(0) > count(1) && mostCommon)
                      || (count(0) <= count(1) && !mostCommon)) 0 else 1
      doFind(index + 1, remainingLines.filter(v => v(index).asDigit == toKeep))
    }
    val remainingLine = doFind(0, lines)
    binToDec(remainingLine.map(_.asDigit))
  }

  def solve(f: Iterable[String] => Int): Int = {
    val lines = Source.fromResource("input3.txt").getLines().toList
    f(lines)
  }

  def solveFirst(): Int = {
    solve(lines => List(true, false).map(getValue(countBits(lines), _)).product)
  }

  def solveSecond(): Int = {
    solve(lines => List(true, false).map(findValue(lines, _)).product)
  }
}
