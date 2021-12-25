package sh.perez.advent21

import scala.util.parsing.combinator.JavaTokenParsers

object Day22 extends Day {
  val day = 22

  case class Point3D(x: Int, y: Int, z: Int) {
    def max(other: Point3D): Point3D = Point3D(x.max(other.x), y.max(other.y), z.max(other.z))
    def min(other: Point3D): Point3D = Point3D(x.min(other.x), y.min(other.y), z.min(other.z))
  }

  case class Cube(start: Point3D, end: Point3D) {
    def intersection(other: Cube): Cube = Cube(start.max(other.start), end.min(other.end))

    def isEmpty: Boolean = start.x > end.x || start.y > end.y || start.z > end.z

    def pointsCount: Long = (end.x - start.x + 1).toLong * (end.y - start.y + 1).toLong * (end.z - start.z + 1).toLong
    
    def splitAround(cube: Cube): List[Cube] = {
      val intersect = this.intersection(cube)
      List(
        Cube(start, Point3D(intersect.start.x - 1, end.y, end.z)),
        Cube(Point3D(intersect.end.x + 1, start.y, start.z), end),
        Cube(Point3D(intersect.start.x, start.y, start.z), Point3D(intersect.end.x, intersect.start.y - 1, end.z)),
        Cube(Point3D(intersect.start.x, intersect.end.y + 1, start.z), Point3D(intersect.end.x, end.y, end.z)),
        Cube(Point3D(intersect.start.x, intersect.start.y, start.z), Point3D(intersect.end.x, intersect.end.y, intersect.start.z - 1)),
        Cube(Point3D(intersect.start.x, intersect.start.y, intersect.end.z + 1), Point3D(intersect.end.x, intersect.end.y, end.z)),
      ).filterNot(_.isEmpty)
    }
  }

  object Grid3D { 
    def empty: Grid3D = Grid3D(Seq.empty)
  }
  case class Grid3D(cubes: Seq[Cube]) {
    def pointsCount: Long = cubes.map(_.pointsCount).sum

    def addCube(cube: Cube): Grid3D = Grid3D(splitAround(cube) :+ cube)

    def removeCube(cube: Cube): Grid3D = Grid3D(splitAround(cube))

    def splitAround(newCube: Cube): Seq[Cube] = {
      cubes.flatMap(c => {
        val intersection = c.intersection(newCube)
        if (intersection.isEmpty) List(c)
        else c.splitAround(newCube)
      })
    }
  }

  case class Instruction(activate: Boolean, cube: Cube)

  object InstructionsParser extends JavaTokenParsers {
    def range(): Parser[(Int, Int)] = (ident ~ "=") ~> (wholeNumber <~ "..") ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    def instruction() =
      ("on" | "off") ~ repsep(range(), ",") ^^ {
        case instr ~ List((x1, x2), (y1, y2), (z1, z2)) =>
          Instruction(instr == "on", Cube(Point3D(x1, y1, z1), Point3D(x2, y2, z2)))
        case _ => throw new RuntimeException("Invalid instruction")
    }

    def parseLine(line: String): Instruction = parseAll(instruction(), line) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw new RuntimeException(failure.msg)
    }
  }

  lazy val instructions = inputLines().map(InstructionsParser.parseLine).toList

  val centerCube = Cube(Point3D(-50, -50, -50), Point3D(50, 50, 50))

  def runInstructions(instructions: List[Instruction]): Long = {
    instructions.foldLeft(Grid3D.empty) {
      case (grid, Instruction(true, cube)) => grid.addCube(cube)
      case (grid, Instruction(false, cube)) => grid.removeCube(cube)
    }.pointsCount
  }

  def solveFirst(): Long = {
    val smallInstructions = instructions.map(i =>
      Instruction(i.activate, i.cube.intersection(centerCube))).filterNot(_.cube.isEmpty)
    runInstructions(smallInstructions)
  }

  def solveSecond(): Long = runInstructions(instructions)
}
