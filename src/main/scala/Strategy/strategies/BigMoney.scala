package Strategy.strategies

import Components.{Card, Gold, Province, Silver}
import Strategy.{BuyingDecisionInfo, Strategy}


case object BigMoney extends Strategy {

  def decideWhatToBuy(info: BuyingDecisionInfo): Vector[Card] =
    if (info.canBuy(Province)) decideWhatToBuy(info.afterBuying(Province))
    else if (info.canBuy(Gold)) decideWhatToBuy(info.afterBuying(Gold))
    else if (info.canBuy(Silver)) decideWhatToBuy(info.afterBuying(Silver))
    else info.boughtSoFar
}