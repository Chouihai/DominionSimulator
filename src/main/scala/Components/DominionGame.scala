package Components

import scala.io.StdIn.readLine
import scala.util.Random

case class DominionGame(supply: Supply,
                        players: Set[Player]){

  val initialSupplySize: Int = supply.size
  var totalPhases: Int = 0
  var active: Boolean = true

  def play: (Set[Player], Int) = {
    Logger.log("Game has started")
    gameLoop(1)
    val winners = getWinners(players)
    Logger.log(players.map(player => s"${player.name}: ${player.victoryPoints}, turns: ${player.getTurns}").mkString("\n"))
    if (winners.size == 1) Logger.log(s"${winners.head.name} wins with ${winners.head.victoryPoints}")
    else Logger.log(s"Game ends in draw between ${winners}")
    (winners, totalPhases)
  }

  private def getWinners(players: Set[Player]): Set[Player] = {
    val highestVictoryPoints = players.toVector.sortBy(_.victoryPoints).last.victoryPoints
    val playersWithHighestVictoryPoints: Vector[Player] = players.filter(player => player.victoryPoints == highestVictoryPoints).toVector
    val leastAmountOfTurns = playersWithHighestVictoryPoints.map(_.getTurns).sorted.head
    if (playersWithHighestVictoryPoints.size > 1) Logger.log(s"$playersWithHighestVictoryPoints have the highest victory points")
    playersWithHighestVictoryPoints.filter(_.getTurns == leastAmountOfTurns).toSet
  }

  private def gameLoop(turn: Int): Unit = if (active) {
    Logger.log(s"Beginning phase $turn")
    if (turn == 1) {
      Logger.log(s"Initializing each players deck to 7 coppers and 3 estates")
      players.foreach(player => {
        val initialDeck = Vector(Copper, Copper, Copper, Copper, Copper, Copper, Copper, Estate, Estate, Estate)
        initialDeck.foreach(supply.take)
        player.initialize(initialDeck)
      })
    }
    val randomizedPlayers = Random.shuffle(players.toVector)
    randomizedPlayers.foreach(player => {
      Logger.log(s"Supply: ${supply.getMap}")
      val boughtAllProvinces = !supply.getMap.contains(Province)
      val usedUpCards = initialSupplySize - supply.size >= 3
      if (boughtAllProvinces) Logger.log(s"Game is over, all provinces have been bought.")
      else if (usedUpCards) Logger.log("Game is over, 3 card stacks have been used up")
      if (boughtAllProvinces || usedUpCards) {
        active = false
        totalPhases = turn
        return gameLoop(turn)
      }
      else {
        Logger.log(s"It is ${player.name}'s turn")
        player.playTurn(players.filterNot(_ == player), supply)
        if (player.name == "Optimal") readLine("press any key to continue:")
      }
    })
    gameLoop(turn + 1)
  }
}

object Logger {

  def log: (String => Unit) = println
}