package sh.perez.advent21

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty || !args(0).forall(c => c.isDigit)) {
      Console.err.println("usage: advent-2021 <day>")
    } else {
      val dayNumber = args(0).toInt
      Day.get(dayNumber) match {
        case Some(day) => {
          val n = System.nanoTime()
          printf("challenge 1: %s\n", day.solveFirst())
          printf("challenge 2: %s\n", day.solveSecond())
          val elapsed = (System.nanoTime() - n) / 1e9
          if (args.length > 1 && args(1) == "--time") {
            printf("completed in %.3fs\n", elapsed)
          }
        }
        case None => Console.err.printf("day %d not solved\n", dayNumber)
      }
    }
  }
}
