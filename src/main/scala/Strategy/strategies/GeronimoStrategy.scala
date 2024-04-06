package Strategy.strategies

import Action.Action._
import Action.Action
import Components._
import Strategy.{ActionDecisionInfo, BuyingDecisionInfo, Strategy}

case object GeronimoStrategy extends Strategy {

  def decideWhatToBuy(info: BuyingDecisionInfo): Vector[Card] =
    if (info.canBuy(Province)) decideWhatToBuy(info.afterBuying(Province))
    else if (info.canBuy(Duchy) && info.supply.provinces <= 3) decideWhatToBuy(info.afterBuying(Duchy))
    else if (info.canBuy(Estate) && info.supply.provinces <= 2) decideWhatToBuy(info.afterBuying(Estate))
    else if (info.canBuy(Gold) && info.playerQuantity(Gold) < 2) decideWhatToBuy(info.afterBuying(Gold))
    else if (info.canBuy(Market) && info.playerQuantity(Market) < 5) decideWhatToBuy(info.afterBuying(Market))
    else if (info.canBuy(Remodel) && !info.playerAlreadyHas(Remodel)) decideWhatToBuy(info.afterBuying(Remodel))
    else if (info.canBuy(Militia) && !info.playerAlreadyHas(Militia)) decideWhatToBuy(info.afterBuying(Militia))
    else if (info.turn <= 2 && info.canBuy(Silver)) decideWhatToBuy(info.afterBuying(Silver))
    else if (info.canBuy(Village) && playerHasLessVillagesThanTerminalActions(info)) decideWhatToBuy(info.afterBuying(Village))
    else if (info.canBuy(Smithy) && !info.playerAlreadyHas(Smithy)) decideWhatToBuy(info.afterBuying(Smithy))
    else if (info.canBuy(Village)) decideWhatToBuy(info.afterBuying(Village))
    else if (info.canBuy(Cellar) && info.playerQuantity(Cellar) < 2) decideWhatToBuy(info.afterBuying(Cellar))
    else info.boughtSoFar


  private def playerHasLessVillagesThanTerminalActions(info: BuyingDecisionInfo): Boolean = {
    val villageQty = info.playerQuantity(Village)
    val terminalActions = info.totalCards.collect {
      case action: Action if action.isTerminal => action
    }.size
    villageQty < terminalActions
  }

  override def decideCardToRemodel(info: ActionDecisionInfo): Option[Card] = {
    if (info.hand.contains(Estate)) Some(Estate)
    else if (info.hand.contains(Gold) && info.supply.provinces <= 2) Some(Gold)
    else None
  }
}