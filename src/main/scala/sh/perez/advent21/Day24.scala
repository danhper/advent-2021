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

  type ResultSet = Set[List[Long]]
  type Results = Map[Int, ResultSet]
  type NextCompute[A] = (Int, Int, Int, Int, Int) => A
  type NextJoin[A] = (A, Long, Results, ResultSet) => Results

  def advance[A](zs: Results, index: Int, computeNext: NextCompute[A], joinNext: NextJoin[A]): Results = {
    val (n1, n2, n3) = extractVars(programs(index))
    (1 to 9).foldLeft(Map.empty[Int, Set[List[Long]]].withDefaultValue(Set.empty[List[Long]])) { (acc, w) => {
      zs.foldLeft(acc) { case (m, (z2, ws)) =>
        val next = computeNext(w, z2, n1, n2, n3)
        joinNext(next, w.toLong, m, ws)
      }
    } }
  }

  def computeNext(zs: Map[Int, Set[List[Long]]], index: Int): Map[Int, Set[List[Long]]] = {
    advance(zs, index, step, (z2, w, m, ws) => m + (z2 -> (m(z2) ++ ws.map(wt => wt :+ w.toLong))))
  }

  def computePrevious(zs: Map[Int, Set[List[Long]]], index: Int): Map[Int, Set[List[Long]]] = {
    advance(zs, index, reverseStep, (zs, w, m, ws) => zs.foldLeft(m) { (m, z) =>
      m + (z -> (m(z) ++ ws.map(wt => w.toLong :: wt)))
    })
  }

  def toDecimal(numbers: List[Long]): Long = numbers.foldLeft(0L) { case (acc, n) => acc * 10 + n }

  // fairly naive with exponential complexity but runs in ~15s on an i7 laptop
  def computePossibilities(): List[Long] = {
    val splitFrontBack = 5
    val initialMap = Map(0 -> Set(List.empty[Long]))
    val fromFront = (0 to splitFrontBack).foldLeft(initialMap)(computeNext)
    val fromBack = (programs.length - 1 until splitFrontBack by -1).foldLeft(initialMap)(computePrevious)
    val intersect = fromFront.keySet.intersect(fromBack.keySet)
    intersect.flatMap(z => for (x <- fromFront(z); y <- fromBack(z)) yield toDecimal(x ++ y)).toList
  }

  lazy val possibilities = computePossibilities()

  def solveFirst(): Long = possibilities.max

  def solveSecond(): Long = possibilities.min
}
