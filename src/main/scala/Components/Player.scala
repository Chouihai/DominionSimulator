package Components

import Action._
import Strategy.{ActionDecisionInfo, BuyingDecisionInfo, Strategy}

class Player(val name: String, val strategy: Strategy) {

  private val deck: Deck = Deck()

  private val hand: Deck = Deck()

  private val discardPile: Deck = Deck()

  private var remainingActions: Int = 0

  private var remainingBuys: Int = 0

  private var turns: Int = 0

  private var activeEffects: Vector[ActionEffect] = Vector.empty

  private var additionalPurchasingPower: Int = 0

  def initialize(cards: Vector[Card]): Unit = {
    turns = 0
    deck.empty
    hand.empty
    discardPile.empty
    activeEffects = Vector.empty
    remainingBuys = 0
    remainingBuys = 0
    deck.setCards(cards)
    deck.shuffle
    draw(5)
  }

  def totalCards: Deck = deck ++ hand ++ discardPile

  override def toString: String = name

  def getTurns: Int = turns

  def victoryPoints: Int = totalCards.getCards.collect { case land: Land => land.victoryPoints }.sum

  def playTurn(opponents: Set[Player], supply: Supply): Unit = {
    turns += 1
    Logger.log(s"$name's hand has ${hand.getCards}")
    val actions = totalCards.getCards.filter(_.isInstanceOf[Action])
    Logger.log(s"$name has ${actions.size} actions in their deck: $actions")
    Logger.log(s"It is turn $turns and $name has this many duchies: ${totalCards.getCards.filter(_ == Duchy)}")
    remainingBuys = 1
    remainingActions = 1
    actionPhase(opponents, supply)
    buyPhase(supply)
    cleanupPhase
  }

  private def cleanupPhase: Unit = {
    remainingBuys = 0
    additionalPurchasingPower = 0
    remainingActions = 0
    discardHand
    draw(5)
  }

  private def purchasingPower: Int = hand.getCards.collect { case treasure: Treasure => treasure.purchasingPower }.sum

  private def actionPhase(opponents: Set[Player], supply: Supply): Unit = {
    val hasAction = hand.find(_.isInstanceOf[Action]).nonEmpty
    if (remainingActions > 0 && hasAction) {
      strategy.decideWhatActionToUse(ActionDecisionInfo(hand, supply, totalCards.getCards)) match {
        case Some(action) => Logger.log(s"$name has decided to use ${action.name}")
          useAction(action, opponents, supply)
          actionPhase(opponents, supply)
        case None => Logger.log(s"$name won't use any of their actions.")
      }
    } else Logger.log(s"$name has no actions to play.")
  }

  private def useAction(action: Action, opponents: Set[Player], supply: Supply): Unit = {
    remainingActions -= 1
    action.effects.foreach {
      case Draw(amount) => val draws = draw(amount)
        Logger.log(s"$name drew $amount card(s): $draws")
      case AdditionalAction(amount) =>
        Logger.log(s"$name gained $amount additional action(s)")
        remainingActions += amount
      case MilitiaEffect => opponents.foreach(_.attackedByMilitia())
      case CellarEffect =>
        val cardsToDiscard = strategy.decideCardsForCellar(Strategy.ActionDecisionInfo(Deck(hand.getCards), new Supply(supply.cardToQty), totalCards.getCards), Vector.empty)
        cardsToDiscard.foreach(discard)
        val draws = draw(cardsToDiscard.size)
        Logger.log(s"$name has decided to discard ${cardsToDiscard.mkString(",")} and drew $draws")
      case AdditionalBuy(amount) =>
        incrementBuys(amount)
        Logger.log(s"$name gained $amount additional buy(s)")
      case AdditionalPurchasingPower(amount) => additionalPurchasingPower += amount
      case MineEffect =>
        val (cardToTrash, result) = strategy.decideCardToMine(hand)
        Logger.log(s"$name decided to trash a $cardToTrash for a $result with his Mine")
        trash(cardToTrash)
        hand.add(result)
      case MerchantEffect => if (hand.contains(Silver)) additionalPurchasingPower += 2
      case RemodelEffect =>
        val cardToRemodel = strategy.decideCardToRemodel(Strategy.ActionDecisionInfo(hand, supply, totalCards.getCards)).get
        val cardToBuy = strategy.decideWhatToBuy(BuyingDecisionInfo(cardToRemodel.price + 2, totalCards.getCards, 1, new Supply(supply.cardToQty), turns, Vector.empty)).headOption
        Logger.log(s"$name remodeled a $cardToRemodel into a $cardToBuy")
        cardToBuy.map(gainCard(_, supply))
        trash(cardToRemodel)
      case WorkshopEffect =>
        val cardToWorkshop = strategy.decideWhatToBuy(Strategy.BuyingDecisionInfo(4, totalCards.getCards, 1, new Supply(supply.cardToQty), turns, Vector.empty)).head
        Logger.log(s"$name used a workshop action to gain a $cardToWorkshop")
        gainCard(cardToWorkshop, supply)
    }
    discard(action)
  }

  private def trash(card: Card): Unit = hand.remove(card)

  private def draw(count: Int): Vector[Card] = drawHelper(count, Vector.empty)

  private def drawHelper(count: Int, accCards: Vector[Card]): Vector[Card] = if (count > 0) {
    if (deck.size > 0) {
      val top = deck.top
      hand.add(top)
      deck.removeTop
      drawHelper(count - 1, accCards :+ top)
    } else {
      discardPile.getCards.foreach(deck.add)
      discardPile.empty
      deck.shuffle
      drawHelper(count, accCards)
    }
  } else accCards


  private def buyPhase(supply: Supply): Vector[Card] = {
    val totalPurchasingPower = purchasingPower + additionalPurchasingPower
    val cardsToBuy = strategy.decideWhatToBuy(Strategy.BuyingDecisionInfo(totalPurchasingPower, totalCards.getCards, remainingBuys, new Supply(supply.cardToQty), turns, Vector.empty))
    Logger.log(s"$name has $totalPurchasingPower total purchasing power and $remainingBuys buys")
    if (cardsToBuy.isEmpty) {
      Logger.log(s"$name decided not to buy anything")
      cardsToBuy
    } else {
      Logger.log(s"$name has decided to buy $cardsToBuy")
      cardsToBuy.foreach(gainCard(_, supply))
      cardsToBuy
    }
  }

  private def gainCard(card: Card, supply: Supply): Unit = {
    discardPile.add(card)
    supply.take(card)
  }


  private def discard(card: Card): Unit = {
    hand.remove(card)
    discardPile.add(card)
  }

  private def discardHand: Unit = {
    hand.getCards.foreach(discard)
    if (hand.size != 0) throw new IllegalStateException("Hand should have nothing")
  }


  def attackedByMilitia(): Unit = {
    val cardsToDiscard = strategy.attackedByMilitia(Deck(hand.getCards), Vector.empty)
    Logger.log(s"$name has been attacked by a Militia! They have decided to discard ${cardsToDiscard.mkString(",")}")
    cardsToDiscard.foreach(discard)
  }

  private def incrementBuys(count: Int): Unit = remainingBuys = remainingBuys + count
}

//case class PlayerState(deck: Deck,
//                       hand: Deck,
//                       discardPile: Deck,
//                       remainingActions: Int,
//                       remainingBuys: Int,
//                       turns: Int,
//                       additionalPurchasingPower: Int)