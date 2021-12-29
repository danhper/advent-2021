package sh.perez.advent21

object Day19 extends Day {
  val day = 19

  object Point3D {
    def fromString(s: String): Point3D = {
      val Array(x, y, z) = s.split(",").map(_.trim.toInt)
      Point3D(x, y, z)
    }

    def origin: Point3D = Point3D(0, 0, 0)
    def rotations: List[Point3D => Point3D] = List(_.id, _.rotate90, _.rotate180, _.rotate270)
    def flips: List[Point3D => Point3D] = List(_.id, _.flip)
    def pivots: List[Point3D => Point3D] = List(_.id, _.pivotLeft, _.pivotUp)

    def transformations: Seq[Point3D => Point3D] =
      for (rotation <- rotations; flip <- flips; pivot <- pivots)
        yield rotation compose flip compose pivot
  }

  case class Point3D(x: Int, y: Int, z: Int) {
    def id: Point3D = this
    def rotate90: Point3D = Point3D(y, -x, z)
    def rotate180: Point3D = rotate90.rotate90
    def rotate270: Point3D = rotate180.rotate90
    def flip: Point3D = Point3D(x, -y, -z)
    def pivotLeft: Point3D = Point3D(-z, y, x)
    def pivotUp: Point3D = Point3D(x, -z, y)

    def distance(other: Point3D): Int = math.abs(x - other.x) + math.abs(y - other.y) + math.abs(z - other.z)
  }

  case class Measurement(id: Int, points: List[Point3D])

  lazy val measurements = {
    inputLines().toList.foldLeft(List(List.empty[Point3D])) { case (acc, line) => {
      line match {
        case l if l.startsWith("---") => acc
        case "" => List.empty[Point3D] :: acc
        case line => (acc.head :+ Point3D.fromString(line)) :: acc.tail
      }
    } }.reverse.zipWithIndex.map((ps, i) => Measurement(i, ps))
  }

  case class Scanner(position: Point3D, measurement: Measurement) {
    val invalidMeasurements = collection.mutable.Set.empty[Int]
    val minIntersectCount = 12
    val normalizedMeasurements = measurement.points.map(p =>
      Point3D(position.x + p.x, position.y + p.y, position.z + p.z)).toSet
    
    def matches(other: Set[Point3D]): Boolean = {
      normalizedMeasurements.intersect(other).size >= minIntersectCount
    }

    def matchMeasurements(right: Measurement): Option[Scanner] = {
      if (invalidMeasurements.contains(right.id)) return None
      val result = (for {
        t <- Stream.from(Point3D.transformations)
        rightTransformed = right.points.map(t)
        r <- Stream.from(rightTransformed)
        l <- Stream.from(normalizedMeasurements)
        scanner = Scanner(Point3D(l.x - r.x, l.y - r.y, l.z - r.z), right.copy(points = rightTransformed))
        if scanner.matches(normalizedMeasurements)
      } yield scanner).headOption
      if (!result.isDefined) invalidMeasurements += right.id
      result
    }
  }

  def computeScanners(measurements: List[Measurement]): List[Scanner] = {
    def loop(scanners: List[Scanner], remainingMeasurements: List[Measurement]): List[Scanner] = {
      remainingMeasurements match {
        case Nil => scanners
        case m :: ms => {
          val newScanner =
            Stream.from(scanners).map(_.matchMeasurements(m)).collect { case Some(v) => v }.headOption
          newScanner match {
            case Some(scanner) => loop(scanner :: scanners, ms)
            case None => loop(scanners, ms :+ m)
          }
        }
      }
    }
    val initialScanners = List(Scanner(Point3D.origin, measurements.head))
    loop(initialScanners, measurements.tail)
  }

  lazy val scanners = computeScanners(measurements)

  def solveFirst(): Int = scanners.flatMap(_.normalizedMeasurements).toSet.size

  def solveSecond(): Int = {
    scanners.flatMap(s => scanners.map(s2 => s.position.distance(s2.position))).max
  }
}
