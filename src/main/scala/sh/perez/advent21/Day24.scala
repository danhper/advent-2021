package sh.perez.advent21

object Day24 extends Day {
  val day = 24

  // this is a translation of the ALU program
  // we make things faster by avoiding the need to "interpret" the program
  def step(w: Int, z: Int, n1: Int, n2: Int, n3: Int): Int = {
    val x = if (z % 26 + n1 == w) 0 else 1
    (z / n2) * (25 * x + 1) + (w + n3) * x
  }

  def reverseStep(w: Int, z2: Int, n1: Int, n2: Int, n3: Int): List[Int] = {
    // case x = 0: z in [z2 * n2, (z2 + 1) * n2)
    val casesX0 = (z2 * n2 until (z2 + 1) * n2).toList
    // case x = 1: z in [n2 * (z2 - w - n3) / 26, n2 * ((z2 - w - n3) / 26 + 1))
    val caseX1Threshold = n2 * (z2 - w - n3) / 26
    val casesX1 = (caseX1Threshold until caseX1Threshold + n2).toList
    (casesX0 ++ casesX1).filter(step(w, _, n1, n2, n3) == z2)
  }

  def extractVars(instrs: List[String]): (Int, Int, Int) = {
    val List(n1, n2, n3) = List(5, 4, 15).map(i => instrs(i).split(" ").last.toInt)
    (n1, n2, n3)
  }

  lazy val programs = inputLines().toList.grouped(18).toList

  def computePrevious(zs: Map[Int, Set[Long]], index: Int): Map[Int, Set[Long]] = {
    val (n1, n2, n3) = extractVars(programs(index))
    (1 to 9).foldLeft(Map.empty[Int, Set[Long]].withDefaultValue(Set.empty[Long])) { (acc, w) => {
      zs.foldLeft(acc) { case (m, (z2, ws)) =>
        reverseStep(w, z2, n1, n2, n3).foldLeft(m) { case (m, z) =>
          m + (z -> (m(z) ++ ws.map(wt => wt + w.toLong * math.pow(10, programs.length - (index + 1)).toLong)))
        }
      }
    } }
  }

  // fairly naive with exponential complexity but runs in ~1m on an i7 laptop
  lazy val possibilities = (programs.length - 1 to 0 by -1).foldLeft(Map(0 -> Set(0L)))(computePrevious)(0)

  def solveFirst(): Long = possibilities.max

  def solveSecond(): Long = possibilities.min
}
