package sh.perez.advent21

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

class Board(val numbers: Array[Array[Int]]) {
  override def toString(): String = {
    numbers.map(t => t.map("%2d".format(_)).mkString(" ")).mkString("\n")
  }

  def wins(drawn: Set[Int]): Boolean = {
    (0 until 5).exists(i =>
      (0 until 5).forall(j => drawn.contains(numbers(i)(j))) ||
      (0 until 5).forall(j => drawn.contains(numbers(j)(i)))
    )
  }

  def computeScore(drawn: Set[Int]): Int = {
    numbers.flatMap(v => v.map(n => if (!drawn.contains(n)) n else 0)).sum
  }
}

trait Day4Parser extends JavaTokenParsers {
  def boardLine(): Parser[Array[Int]] = repN(5, wholeNumber) ^^ { _.map(_.toInt).toArray }
  def board(): Parser[Board] = repN(5, boardLine()) ^^ { x => Board(x.toArray) }
  def boards(): Parser[List[Board]] = rep(board())
  def draw(): Parser[List[Int]] = repsep(wholeNumber, ",") ^^ { _.map(_.toInt) }
  def input(): Parser[Tuple2[List[Int], List[Board]]] = draw() ~ boards() ^^ { case d ~ t => (d, t) }
}

object Day4 extends Day with Day4Parser {
  def findFirstWinner(draw: List[Int], boards: List[Board]): (List[Int], Board) = {
    val reducedDraw = draw.inits.toList.reverse.find(d => boards.exists(_.wins(d.toSet))).get
    (reducedDraw, boards.find(_.wins(reducedDraw.toSet)).get)
  }

  def findLastWinner(draw: List[Int], boards: List[Board]): (List[Int], Board) = {
    val reducedDraw = draw.inits.toList.reverse.find(d => boards.forall(_.wins(d.toSet))).get
    (reducedDraw, boards.find(!_.wins(reducedDraw.dropRight(1).toSet)).get)
  }

  def solve(findBoard: (List[Int], List[Board]) => (List[Int], Board)): Int = {
    val inputReader = Source.fromResource("input4.txt").reader
    val Success((draw, boards), _) = parseAll(input(), inputReader)
    val (smallestDraw, board) = findBoard(draw, boards)
    board.computeScore(smallestDraw.toSet) * smallestDraw.last
  }

  def solveFirst(): Int = solve(findFirstWinner)

  def solveSecond(): Int = solve(findLastWinner)
}
