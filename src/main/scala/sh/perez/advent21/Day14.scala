package sh.perez.advent21

object Day14 extends Day {
  val day = 14

  class Polymer(val template: String, val counts: Map[String, Long], val insertions: Map[String, String]) {
    def step(): Polymer = {
      val newCounts = counts.foldLeft(Map.empty[String, Long]) { case (m, (v, c)) => 
        val i = insertions(v)
        val (left, right) = (v(0) + i, i + v(1))
        m + (left -> (c + m.getOrElse(left, 0L))) + (right -> (c + m.getOrElse(right, 0L)))
      }
      new Polymer(template, newCounts, insertions)
    }

    override def toString(): String = counts.toString()

    def countElements(): Map[Char, Long] = {
      val elems = counts.foldLeft(Map.empty[Char, Long]) { case (m, (v, count)) => {
        m + (v(0) -> (m.getOrElse(v(0), 0L) + count))
      } }
      elems + (template.last -> (elems.getOrElse(template.last, 0L) + 1L))
    }
  }

  lazy val polymer = {
    val (List(template), rawInsertions) = inputLines().toList.splitAt(1)
    val insertions = rawInsertions.tail.foldLeft(Map.empty[String, String]) { case (m, i) =>
      val Array(k, v) = i.split(" -> ")
      m + (k -> v)
    }
    val initialCounts = template.sliding(2).foldLeft(Map.empty[String, Long]) { case (m, i) =>
      m + (i -> (m.getOrElse(i, 0L) + 1L))
    }
    new Polymer(template, initialCounts, insertions)
  }

  def solve(n: Int): Long = {
    val newPolymer = (0 until n).foldLeft(polymer) { case (p, _) => p.step() }
    val elems = newPolymer.countElements()
    elems.values.max - elems.values.min
  }

  def solveFirst(): Long = solve(10)

  def solveSecond(): Long = solve(40)
}
