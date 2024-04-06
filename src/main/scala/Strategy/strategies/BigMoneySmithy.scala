package Strategy.strategies

import Action.Action.Smithy
import Components._
import Strategy.{BuyingDecisionInfo, Strategy}

case object BigMoneySmithy extends Strategy {

  def decideWhatToBuy(info: BuyingDecisionInfo): Vector[Card] =
    if (info.canBuy(Province)) decideWhatToBuy(info.afterBuying(Province))
    else if (info.canBuy(Duchy) && info.turn > 12) decideWhatToBuy(info.afterBuying(Duchy))
    else if (info.canBuy(Gold)) decideWhatToBuy(info.afterBuying(Gold))
    else if (info.canBuy(Smithy) && !info.playerAlreadyHas(Smithy)) decideWhatToBuy(info.afterBuying(Smithy))
    else if (info.canBuy(Silver)) decideWhatToBuy(info.afterBuying(Silver))
    else info.boughtSoFar
}
