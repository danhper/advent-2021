package sh.perez.advent21

import scala.io.Source

trait Instruction
case class Forward(x: Int) extends Instruction
case class Up(x: Int) extends Instruction
case class Down(x: Int) extends Instruction

object Day2 extends Day {
  def parseInstruction(s: String): Instruction = {
    val Array(instr, n) = s.split(" ", 2)
    instr match {
      case "up"      => Up(n.toInt)
      case "down"    => Down(n.toInt)
      case "forward" => Forward(n.toInt)
      case _         => throw new Exception(s"Unknown instruction: $instr")
    }
  }

  def solve(f: (Iterator[Instruction] => (Int, Int))): Int = {
    val instrs = Source.fromResource("input2.txt").getLines().map(parseInstruction)
    val (x, y) = f(instrs)
    x * y
  }

  def solveFirst(): Int = solve(instrs => {
    instrs.foldLeft((0, 0)) {
      case ((x, y), Forward(n)) => (x + n, y)
      case ((x, y), Up(n))      => (x, y - n)
      case ((x, y), Down(n))    => (x, y + n)
    }
  })

  def solveSecond(): Int = solve(instrs => {
    val (x, y, aim) = instrs.foldLeft((0, 0, 0)) {
      case ((x, y, aim), Forward(n)) => (x + n, y + n * aim, aim)
      case ((x, y, aim), Up(n))      => (x, y, aim - n)
      case ((x, y, aim), Down(n))    => (x, y, aim + n)
    }
    (x, y)
  })
}
