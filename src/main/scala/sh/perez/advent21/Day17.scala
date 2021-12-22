package sh.perez.advent21

object Day17 extends Day {
  val day = 17

  lazy val (start, end) = {
    val toDrop = "target area: ".length
    val Array(x, y) = inputFirstLine().drop(toDrop).split(",").map(_.trim().drop(2).split("\\.\\."))
    (Point2D(x(0).toInt, y(1).toInt), Point2D(x(1).toInt, y(0).toInt))
  }

  def simulate(velocity: Point2D): (Point2D, Point2D) = {
    def runSimulation(pos: Point2D, highest: Point2D, vel: Point2D): (Point2D, Point2D) = {
      val newPos = Point2D(pos.x + vel.x, pos.y + vel.y)
      if (newPos.x > end.x || newPos.y < end.y) (pos, highest)
      else {
        val newVel = Point2D(vel.x - math.signum(vel.x), vel.y - 1)
        val newHighest = if (newPos.y > highest.y) newPos else highest
        runSimulation(newPos, newHighest, newVel)
      }
    }
    runSimulation(Point2D(0, 0), Point2D(0, 0), velocity)
  }

  def isInTarget(pos: Point2D): Boolean = {
    pos.x >= start.x && pos.x <= end.x && pos.y <= start.y && pos.y >= end.y
  }

  lazy val minXVelocity: Int = math.ceil((-1 + math.sqrt(1 + 8 * start.x)) / 2.0).toInt
  val maxYVelocity: Int = 1000

  def getAllVelocities(): Seq[Point2D] = {
    for (x <- minXVelocity to minXVelocity + end.x; y <- end.y to maxYVelocity)
      yield Point2D(x, y)
  }

  def solveFirst(): Int = {
    getAllVelocities().foldLeft(Point2D(0, 0)) { (highestPos, velocity) => 
      val (pos, highest) = simulate(velocity)
      if (isInTarget(pos) && highest.y > highestPos.y) highest else highestPos
    }.y
  }

  def solveSecond(): Int = {
    getAllVelocities().foldLeft(Set.empty[Point2D]) { (velocities, velocity) =>
      val (pos, highest) = simulate(velocity)
      if (isInTarget(pos)) velocities + velocity else velocities
    }.size
  }
}
