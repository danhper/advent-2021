package sh.perez.advent21

object Main {

  val days: Map[Int, Day] = Map(
    1 -> Day1,
    2 -> Day2
  )

  def main(args: Array[String]): Unit = {
    if (args.isEmpty || !args(0).forall(c => c.isDigit)) {
      Console.err.println("usage: advent-2021 <day>")
    } else {
      val day = args(0).toInt
      if (!days.contains(day)) {
        Console.err.printf("day %d not solved", day)
      } else {
        val dayLogic = days(day)
        printf("challenge 1: %s\n", dayLogic.solveFirst())
        printf("challenge 2: %s\n", dayLogic.solveSecond())
      }
    }
  }
}
