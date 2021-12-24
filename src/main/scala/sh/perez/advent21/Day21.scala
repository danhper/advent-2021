package sh.perez.advent21

object Day21 extends Day {
  val day = 21

  class DeterministicDice {
    var rolls: Int = 0
    var currentValue: Int = 1

    def roll(n: Int): Int = (1 to n).map(_ => roll()).sum
    def roll(): Int = {
      val value = currentValue
      rolls += 1
      currentValue += 1
      if (currentValue > 100) currentValue = 1
      value
    }
  }

  trait Mergeable[A <: Mergeable[A, B], B] {
    def count: Long
    def uniqueId: B
    def step(mergeable: Roll): A
    def withCount(newCount: Long): A
  }

  case class Roll(val value: Int, val count: Long) extends Mergeable[Roll, Int] {
    val uniqueId: Int = value

    def withCount(newCount: Long): Roll = Roll(value, newCount)
    def step(roll: Roll): Roll = {
      val newCount = count * roll.count
      val newValue = value + roll.value
      Roll(newValue, newCount)
    }
  }


  case class Game(val player1: Player, val player2: Player, val maxScore: Int,
                  val p1Turn: Boolean = true, val count: Long = 1L) extends Mergeable[Game, (Int, Int, Int, Int)] {
    val uniqueId: (Int, Int, Int, Int) = (player1.score, player1.position, player2.score, player2.position)
    def step(roll: Roll): Game = {
      if (p1Turn) Game(player1.turn(roll.value), player2, maxScore, false, count * roll.count)
      else Game(player1, player2.turn(roll.value), maxScore, true, count * roll.count)
    }
    def withCount(newCount: Long): Game = Game(player1, player2, maxScore, p1Turn, newCount)

    def looser: Player = if (player1.score > player2.score) player2 else player1

    def done: Boolean = player1.score >= maxScore || player2.score >= maxScore
  }

  def combine[A <: Mergeable[A, B], B](left: List[A], right: List[Roll]): List[A] = {
    left.foldLeft(Map.empty[B, A]) { (m, l) => {
      right.foldLeft(m) { (m, r) => {
        val next = l.step(r)
        val currentCount = m.get(next.uniqueId).map(_.count).getOrElse(0L)
        m + (next.uniqueId -> next.withCount(next.count + currentCount))
      }}
    } }.values.toList
  }

  lazy val basicDiracRoll = List(1, 2, 3).map(Roll(_, 1))
  lazy val diracRoll = combine(combine(basicDiracRoll, basicDiracRoll), basicDiracRoll)

  case class Player(val position: Int, val turns: Int = 0, val score: Int = 0) {
    def turn(value: Int): Player = {
      var newPosition = position + value
      newPosition = if (newPosition % 10 == 0) 10 else newPosition % 10
      Player(newPosition, turns + 1, score + newPosition)
    }
  }

  def playNormal(game: Game): (Player, DeterministicDice) = {
    def play(game: Game, dice: DeterministicDice): Game = {
      if (game.done) game
      else play(game.step(Roll(dice.roll(3), 1)), dice)
    }
    val dice = new DeterministicDice()
    val endGame = play(game, dice)
    (endGame.looser, dice)
  }

  def playDirac(games: List[Game]): List[Game] = {
    def nextRound(games: List[Game]): List[Game] = {
      val (doneGames, runningGames) = games.partition(_.done)
      doneGames ++ combine(runningGames, diracRoll)
    }

    if (games.forall(_.done)) games
    else playDirac(nextRound(games))
  }

  lazy val (player1, player2) = {
    val inputString = "Player 1 starting position: "
    val List(p1Position, p2Position) = inputLines().map(_.drop(inputString.length).toInt).toList
    (Player(p1Position), Player(p2Position))
  }

  def solveFirst(): Long = {
    val (looser, dice) = playNormal(Game(player1, player2, 1000))
    looser.score * dice.rolls
  }

  def solveSecond(): Long = {
    val games = playDirac(List(Game(player1, player2, 21)))
    val (p1Wins, p2Wins) = games.partition(g => g.player1.turns <= g.player2.turns)
    List(p1Wins, p2Wins).map(_.map(_.count).sum).max
  }
}
