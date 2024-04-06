import Action.Action.BaseActionSet
import Components._
import Strategy.strategies.{BigMoney, BigMoneySmithy, BigMoneySmithyMarket, GeronimoStrategy}

object Main extends App {
  val actionSupplyMap: Map[Card, Int] = BaseActionSet.map(action => (action -> 10)).toMap

  val REPETITIONS = 100

  val supplyMap: Map[Card, Int] = (Map(
    Copper -> 60,
    Silver -> 40,
    Gold -> 30,
    Estate -> 24,
    Duchy -> 12,
    Province -> 12
  ) ++ actionSupplyMap).toMap


  val Player1 = new Player("Big Money", BigMoney)
  val Player2 = new Player("Big Money Smithy", BigMoneySmithy)
  val Player3 = new Player("BMSM", BigMoneySmithyMarket)
  val Player4 = new Player("Geronimo", GeronimoStrategy)

  var player4Wins = 0.0
  var player1Wins = 0.0
  var player2Wins = 0.0
  var player3Wins = 0.0
  var phasesTotal = 0.0
  Vector.range(0, REPETITIONS).foreach(_ => {
    val inventory = new Supply(supplyMap)
    val game = DominionGame(inventory, Set(Player1, Player2, Player3, Player4))
    val (winners, phases) = game.play
    phasesTotal += phases
    if (winners.size == 1) winners.head match {
      case Player4 => player4Wins += 1
      case Player2 => player2Wins += 1
      case Player1 => player1Wins += 1
      case Player3 => player3Wins += 1
    }
    else winners.foreach {
      case Player4 => player4Wins += 0.5
      case Player2 => player2Wins += 0.5
      case Player1 => player1Wins += 0.5
      case Player3 => player3Wins += 0.5
    }
  })
  Logger.log(s"Wins after 100 games: ${Player4.name}($player4Wins), ${Player1.name}($player1Wins), ${Player2.name}($player2Wins), ${Player3.name}($player3Wins), average turns: ${phasesTotal/REPETITIONS}")
}