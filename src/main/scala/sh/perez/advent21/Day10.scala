package sh.perez.advent21

object Day10 extends Day {
  val day = 10

  val parenthesisMapping = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  val errorPoints = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val completePoints = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  lazy val inputs = inputLines().map(parseLine).toList

  def parseLine(line: String): Either[Char, List[Char]] = {
    val remaining = line.foldLeft(List.empty[Char]) { case (acc, c) => {
      acc match {
        case _ if parenthesisMapping.contains(c) => c :: acc
        case head :: tail if parenthesisMapping(head) == c => tail
        case _ => return Left(c)
      }
    } }
    Right(remaining)
  }

  def computeAutocompleteScore(remaining: List[Char]): Long = {
    remaining.foldLeft(0L) { case (acc, c) => {
      acc * 5L + completePoints(parenthesisMapping(c))
    } }
  }

  def solveFirst(): Int = {
    inputs.collect { case Left(v) => errorPoints(v) }.sum
  }

  def solveSecond(): Long = {
    val results = inputs.collect { case Right(v) => computeAutocompleteScore(v) }.sorted
    results(results.length / 2)
  }
}
