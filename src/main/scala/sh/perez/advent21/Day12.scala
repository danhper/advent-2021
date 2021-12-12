package sh.perez.advent21


object Day12 extends Day {
  val day = 12

  lazy val edges = inputLines().map(v => {
    val Array(from, to) = v.split("-")
    (from, to)
  }).foldLeft(Map.empty[String, Set[String]].withDefaultValue(Set.empty[String])) {
    case (acc, (from, to)) => acc + (from -> (acc(from) + to)) + (to -> (acc(to) + from))
  }

  def isSmallCave(node: String): Boolean = node.forall(_.isLower)

  def findPaths(allowDoubleSeen: Boolean): Set[List[String]] = {
    def findAllPaths(currentNode: String, visited: Set[String], doubleSmall: Option[String]): Set[List[String]] = {
      if (currentNode == "end") Set(List(currentNode))
      else {
        val (nextVisited, nextDoubleSmall) = (doubleSmall, isSmallCave(currentNode)) match {
          case (Some(small), _) if small == currentNode => (visited, None)
          case (_, true) => (visited + currentNode, doubleSmall)
          case (_, false) => (visited, doubleSmall)
        }
        edges(currentNode).filterNot(visited).flatMap(nextNode => {
          findAllPaths(nextNode, nextVisited, nextDoubleSmall).map(currentNode :: _)
        })
      }
    }
    if (allowDoubleSeen) {
      val smallCaves = edges.keys.filter(v => isSmallCave(v) && v != "start" && v != "end")
      smallCaves.flatMap(node => findAllPaths("start", Set.empty[String], Some(node))).toSet
    } else {
      findAllPaths("start", Set.empty[String], None)
    }
  }

  def solveFirst(): Int = findPaths(false).size

  def solveSecond(): Int = findPaths(true).size
}
