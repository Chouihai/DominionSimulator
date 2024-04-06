package Strategy

import Action.Action._
import Action.Action
import Components._

import scala.util.Random

trait Strategy {

  def decideWhatToBuy(buyingDecisionInfo: BuyingDecisionInfo): Vector[Card]

  /**
   * Default strategy is to start if with non terminal actions first, if none select at random
   */
  def decideWhatActionToUse(info: ActionDecisionInfo): Option[Action] = {
    val actions = info.hand.getCards.filter(_.isInstanceOf[Action]).asInstanceOf[Vector[Action]]
    val nonTerminalActions = actions.filterNot(_.isTerminal)
    val maybeAction = if (actions.isEmpty) None
    else if (actions.size == 1) actions.headOption
    else if (nonTerminalActions.size > 0) nonTerminalActions.headOption
    // TODO: change it so that it cycles through all actions, should not return None unless it has checked all of them.
    else Random.shuffle(actions).headOption
    verifyIfValidAction(maybeAction, info)
  }

  private def verifyIfValidAction(maybeAction: Option[Action], info: ActionDecisionInfo): Option[Action] = maybeAction match {
    case Some(action) => action match {
      case Remodel if decideCardToRemodel(info).isEmpty => None
      case Mine if !(info.hand.contains(Silver) || info.hand.contains(Copper)) => None
      case Cellar if decideCardsForCellar(info, Vector.empty).isEmpty => None
      case _ => Some(action)
    }
    case None => None
  }


  /**
   * Decides which cards to discard when attacked by Militia
   */
  def attackedByMilitia(hand: Deck, accCards: Vector[Card]): Vector[Card] = if (hand.size > 3) {
    if (hand.contains(Estate)) attackedByMilitia(hand.remove(Estate), accCards :+ Estate)
    else if (hand.contains(Copper)) attackedByMilitia(hand.remove(Copper), accCards :+ Copper)
    else {
      val top = hand.top
      attackedByMilitia(hand.removeTop, accCards :+ top)
    }
  } else accCards

  /**
   * Decides which cards to discard if playing a cellar
   *
   * Default behavior is to just get rid of all the land cards
   */
  def decideCardsForCellar(info: ActionDecisionInfo, accCards: Vector[Card]): Vector[Card] = if (info.hand.size > 1) {
    info.hand.find(_.isInstanceOf[Land]) match {
      case Some(card) => decideCardsForCellar(info.afterDiscarding(card), accCards :+ card)
      case None => accCards
    }
  } else accCards

  /**
   * Default is to mine copper if I have a copper, and a silver if I have a silver
  */
  def decideCardToMine(hand: Deck): (Card, Card) = {
    if (hand.contains(Copper)) Copper -> Silver
    else if (hand.contains(Silver)) Silver -> Gold
    else throw new IllegalStateException("Nothing to mine")
  }

  /**
   * Default will remodel estates
   */
  def decideCardToRemodel(info: ActionDecisionInfo): Option[Card] = {
    if (info.hand.contains(Estate)) Some(Estate)
    else None
  }
}


case class BuyingDecisionInfo(purchasingPower: Int,
                              totalCards: Vector[Card],
                              availableBuys: Int,
                              supply: Supply,
                              turn: Int,
                              boughtSoFar: Vector[Card]) {

  def afterBuying(card: Card): BuyingDecisionInfo = {
    supply.take(card)
    copy(purchasingPower = purchasingPower - card.price, availableBuys = availableBuys - 1, boughtSoFar = boughtSoFar :+ card)
  }

  def canBuy(card: Card): Boolean = purchasingPower >= card.price && availableBuys > 0 && supply.contains(card) && supply.contains(Province)

  def playerAlreadyHas(card: Card): Boolean = totalCards.contains(card)

  def playerQuantity(card: Card): Int = totalCards.filter(_ == card).size
}

case class ActionDecisionInfo(hand: Deck, supply: Supply, totalCards: Vector[Card]) {

  def afterDiscarding(card: Card): ActionDecisionInfo = copy(hand.remove(card))
}