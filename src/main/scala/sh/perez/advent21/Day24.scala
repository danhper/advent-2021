package sh.perez.advent21

import scala.util.parsing.combinator.JavaTokenParsers

object Day24 extends Day {
  val day = 24

  case class Env(input: List[Int], registers: Map[Char, Int] = Map.empty.withDefaultValue(0))
  trait Value {
    def eval(env: Env): Int
  }
  case class Literal(value: Int) extends Value {
    def eval(env: Env): Int = value
  }
  case class Variable(name: Char) extends Value {
    def eval(env: Env): Int = env.registers(name)
  }

  def applyBinary(env: Env, a: Variable, b: Value, op: (Int, Int) => Int) = {
    env.copy(registers = env.registers.updated(a.name, op(a.eval(env), b.eval(env))))
  }

  trait Instruction {
    def eval(env: Env): Env
  }

  case class Input(name: Char) extends Instruction {
    def eval(env: Env): Env = {
      Env(input = env.input.drop(1), registers = env.registers + (name -> env.input.head))
    }
  }
  case class Add(a: Variable, b: Value) extends Instruction {
    def eval(env: Env): Env = applyBinary(env, a, b, (a, b) => a + b)
  }
  case class Sub(a: Variable, b: Value) extends Instruction {
    def eval(env: Env): Env = applyBinary(env, a, b, (a, b) => a - b)
  }
  case class Mul(a: Variable, b: Value) extends Instruction {
    def eval(env: Env): Env = applyBinary(env, a, b, (a, b) => a * b)
  }
  case class Div(a: Variable, b: Value) extends Instruction {
    def eval(env: Env): Env = applyBinary(env, a, b, (a, b) => a / b)
  }
  case class Mod(a: Variable, b: Value) extends Instruction {
    def eval(env: Env): Env = applyBinary(env, a, b, (a, b) => {
      if (a < 0 || b <= 0) throw new ArithmeticException("Modulo with zero or negative")
      a % b
    })
  }
  case class Eql(a: Variable, b: Value) extends Instruction {
    def eval(env: Env): Env = applyBinary(env, a, b, (a, b) => if (a == b) 1 else 0)
  }

  object InstructionParser extends JavaTokenParsers {
    def instruction: Parser[Instruction] = input | binaryOp
    def variable: Parser[Variable] = ident ^^ { name => Variable(name.head) }
    def literal: Parser[Literal] = wholeNumber ^^ { n => Literal(n.toInt) }
    def value: Parser[Value] = variable | literal
    def input: Parser[Day24.Input] = "inp" ~> ident ^^ { name => Input(name.head) } 
    def binaryOp: Parser[Instruction] = ident ~ variable ~ value ^^ { case op ~ a ~ b => op match {
      case "add" => Add(a, b)
      case "sub" => Sub(a, b)
      case "mul" => Mul(a, b)
      case "div" => Div(a, b)
      case "mod" => Mod(a, b)
      case "eql" => Eql(a, b)
    } }
    def parseInstruction(input: String): Instruction = parseAll(instruction, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw new Exception(failure.msg)
    }
    def parseInstructions(input: List[String]): List[Instruction] = input.map(parseInstruction)
  }

  def runProgram(env: Env, instrs: List[Instruction]): Env = {
    instrs.foldLeft(env)((env, instr) => instr.eval(env))
  }

  def step(w: Int, z: Int, n1: Int, n2: Int, n3: Int): Int = {
    val x = if (z % 26 + n1 == w) 0 else 1
    (z / n2) * (25 * x + 1) + (w + n3) * x
  }

  lazy val instrs = InstructionParser.parseInstructions(inputLines().toList)

  // def extractVars(instrs: List[Instruction]): (Int, Int, Int) = {
  //   (instrs(5).asInstanceOf[Add].b.asInstanceOf[Literal].value,
  //    instrs(4).asInstanceOf[Div].b.asInstanceOf[Literal].value,
  //    instrs(15).asInstanceOf[Add].b.asInstanceOf[Literal].value)
  // }

  def extractVars(instrs: List[String]): (Int, Int, Int) = {
    val List(n1, n2, n3) = List(5, 4, 15).map(i => instrs(i).split(" ").last.toInt)
    (n1, n2, n3)
  }


  def runStep(w: Int, z: Int, instrs: List[String]): Int = {
    val (n1, n2, n3) = extractVars(instrs)
    step(w, z, n1, n2, n3)
  }

  def findPossibilities(): List[Long] = {
    val grouped = inputLines().toList.grouped(18).toList
    val zss = grouped.dropRight(1).zipWithIndex.scanLeft(Set(0)) { case (zs, (group, i)) =>
      println(s"running group $i")
      (1 to 9).flatMap(i => zs.map(z => runStep(i, z, group))).toSet
    }

    val recovered = zss.zip(grouped).reverse.zipWithIndex.foldLeft(List((0L, 0))) { case (acc, ((zs, group), i)) => {
      println(s"running group ${grouped.length - (i + 1)}")
      val acceptable = acc.map(_._2).toSet
      (1 to 9).flatMap(w => {
        zs.map(z => (z, runStep(w, z, group))).filter((z, z2) => acceptable.contains(z2)).flatMap((z, z2) => {
          val existing = acc.filter(_._2 == z2)
          existing.map((i0, _) => (i0 + w.toLong * math.pow(10, i).toLong, z))
        })
      }).toList
    } }

    recovered.map(_._1)
  }

  lazy val possibilities = findPossibilities()

  def solveFirst(): Long = possibilities.max

  def solveSecond(): Long = possibilities.min
}
