package sh.perez.advent21

object Day24 extends Day {
  val day = 24

  // this is a translation of the ALU program
  // we make things faster by avoiding the need to "interpret" the program
  def step(w: Int, z: Int, n1: Int, n2: Int, n3: Int): Int = {
    val x = if (z % 26 + n1 == w) 0 else 1
    (z / n2) * (25 * x + 1) + (w + n3) * x
  }

  def extractVars(instrs: List[String]): (Int, Int, Int) = {
    val List(n1, n2, n3) = List(5, 4, 15).map(i => instrs(i).split(" ").last.toInt)
    (n1, n2, n3)
  }

  // very naive and inefficient but finishes in ~5m on a normal laptop
  def findPossibilities(): List[Long] = {
    val grouped = inputLines().toList.grouped(18).toList
    val zss = grouped.dropRight(1).scanLeft(Set(0)) { case (zs, group) =>
      val (n1, n2, n3) = extractVars(group)
      (1 to 9).flatMap(w => zs.map(z => step(w, z, n1, n2, n3))).toSet
    }

    val recovered = zss.zip(grouped).reverse.zipWithIndex.foldLeft(List((0L, 0))) { case (acc, ((zs, group), i)) => {
      val acceptable = acc.map(_._2).toSet
      val (n1, n2, n3) = extractVars(group)
      (1 to 9).flatMap(w => {
        zs.map(z => (z, step(w, z, n1, n2, n3))).filter((z, z2) => acceptable.contains(z2)).flatMap((z, z2) => {
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
