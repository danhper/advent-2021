package sh.perez.advent21


object Day20 extends Day {
  val day = 20

  type Image = Set[Point2D]

  val border = 3

  def binToDec(value: Iterable[Int]): Int = value.foldLeft(0)(_ * 2 + _)

  lazy val (imageEnhancement, initialImage) = {
    val (rawEnhancement :: _ :: rawImageGrid) = inputLines().toList
    val imageGrid = rawImageGrid.zipWithIndex.foldLeft(Set.empty[Point2D]) { case (image, (line, y)) => {
      line.zipWithIndex.foldLeft(image) { case (img, (char, x)) =>
        if (char == '#') img + Point2D(x, y) else img
      }
    } }
    val enhancement = rawEnhancement.zipWithIndex.foldLeft(Map.empty[Int, Int]) { case (mapping, (char, i)) =>
      mapping + (i -> (if (char == '#') 1 else 0))
    }
    (enhancement, imageGrid)
  }

  def getNeighbors(point: Point2D): Seq[Point2D] = {
    for (y <- point.y - 1 to point.y + 1; x <- point.x - 1 to point.x + 1)
      yield Point2D(x, y)
  }

  def getBounds(image: Image) = {
    val (startX, endX) = (image.minBy(_.x).x, image.maxBy(_.x).x)
    val (startY, endY) = (image.minBy(_.y).y, image.maxBy(_.y).y)
    ((startX, endX), (startY, endY))
  }

  def getPointsInArea(image: Image): Seq[Point2D] = {
    val ((startX, endX), (startY, endY)) = getBounds(image)
    for (x <- startX - border to endX + border; y <- startY - border to endY + border) yield Point2D(x, y)
  }

  def enhanceImage(image: Image): Image = {
    getPointsInArea(image).foldLeft(Set.empty[Point2D]) { (newImage, point) => {
      if (imageEnhancement(getOffset(image, point)) == 1) newImage + point else newImage
    } }
  }

  def trimImage(image: Image): Image = {
    val ((startX, endX), (startY, endY)) = getBounds(image)
    if (imageEnhancement(0) == 0) image
    else image.filter(point => point.x > startX + border && point.x < endX - border && point.y > startY + border && point.y < endY - border)
  }

  def getOffset(image: Image, point: Point2D): Int = {
    val neighbors = getNeighbors(point)
    val bits = neighbors.map(p => if (image.contains(p)) 1 else 0)
    binToDec(bits)
  }

  def solve(n: Int): Int = {
    (1 to n / 2).foldLeft(initialImage) { (image, _) => trimImage(enhanceImage(enhanceImage(image))) }.size
  }

  def solveFirst(): Int = solve(2)

  def solveSecond(): Int = solve(50)
}
