package Action

import Components.Card

case class Action(name: String, price: Int, effects: Vector[ActionEffect]) extends Card {

  def isTerminal: Boolean = effects.find(_.isInstanceOf[AdditionalAction]).isEmpty
}

object Action {
  val Village = Action("Village", 3, Vector(Draw(1), AdditionalAction(2)))
  val Smithy = Action("Smithy", 4, Vector(Draw(3)))
  val Workshop = Action("Workshop", 3, Vector(WorkshopEffect))
  val Militia = Action("Militia", 4, Vector(AdditionalPurchasingPower(2),MilitiaEffect))
  val Merchant = Action("Merchant", 3, Vector(Draw(1), AdditionalAction(1), MerchantEffect))
  val Cellar = Action("Cellar", 2, Vector(CellarEffect, AdditionalAction(1)))
  val Market = Action("Market", 5, Vector(AdditionalAction(1), AdditionalBuy(1), AdditionalPurchasingPower(1), Draw(1)))
  val Remodel = Action("Remodel", 4, Vector(RemodelEffect))
  val Moat = Action("Moat", 2, Vector(Draw(2)))
  val Mine = Action("Mine", 5, Vector(MineEffect))
  val BaseActionSet: Set[Card] = Set(Village, Smithy, Workshop, Market, Militia, Cellar, Merchant, Remodel, Moat, Mine)
}

sealed trait ActionEffect {
  var active: Boolean = false

  def setActive(boolean: Boolean): Unit = active = boolean
}

case class Draw(amount: Int) extends ActionEffect

case class AdditionalAction(amount: Int) extends ActionEffect

case object WorkshopEffect extends ActionEffect

case object MilitiaEffect extends ActionEffect

case object CellarEffect extends ActionEffect

case class AdditionalBuy(amount: Int) extends ActionEffect

case class AdditionalPurchasingPower(amount: Int = 1) extends ActionEffect

case object MineEffect extends ActionEffect

case object MerchantEffect extends ActionEffect

case object RemodelEffect extends ActionEffect

//sealed trait
//
//case object Village extends Action {
//
//  override def effect(player: Player, opponents: Set[Player]): Unit = {
//    player.draw(1)
//    player.incrementAvailableActions(2)
//  }
//
//  val price: Int = 3
//  val isTerminal: Boolean = false
//}
//
//case object Smithy extends Action {
//
//  def effect(player: Player, opponents: Set[Player]): Unit = player.draw(3)
//
//  val price: Int = 4
//  val isTerminal: Boolean = true
//}
//
//case object Workshop extends Action {
//
//  def effect(player: Player, opponents: Set[Player]): Unit = ???
//
//  val price: Int = 3
//  val isTerminal: Boolean = ???
//}
//
//case object Militia extends Action {
//
//  def effect(player: Player, opponents: Set[Player]): Unit = {
//    opponents.foreach(_.attackedByMilitia())
//    player.incrementPurchasingPower(2)
//  }
//
//  val price: Int = 4
//  val isTerminal: Boolean = true
//}
//
//case object Merchant extends Action {
//
//  override def effect(player: Player, opponents: Set[Player]): Unit = {
//    player.draw(1)
//    player.incrementAvailableActions(1)
//  }
//
//  override val price: Int = 3
//  val isTerminal: Boolean = false
//}
//
//case object Cellar extends Action {
//
//  override protected def effect(player: Player, opponents: Set[Player]): Unit = {
//    player.incrementAvailableActions(1)
//    player.strategy.discardForCellar()
//  }
//
//  override val price: Int = 2
//  val isTerminal: Boolean = false
//}
//
//case object Market extends Action {
//
//  override protected def effect(player: Player, opponents: Set[Player]): Unit = {
//    player.draw(1)
//    player.incrementAvailableActions(1)
//    player.incrementPurchasingPower(1)
//    player.incrementBuys(1)
//  }
//
//  override val price: Int = 5
//  val isTerminal: Boolean = false
//}
//
//case object Remodel extends Action {
//
//  override protected def effect(player: Player, opponents: Set[Player]): Unit = ???
//
//  override val price: Int = 4
//  val isTerminal: Boolean = true
//}
//
//case object Moat extends Action {
//
//  protected def effect(player: Player, opponents: Set[Player]): Unit = player.draw(2)
//
//  val price: Int = 2
//  val isTerminal: Boolean = true
//}
//
//case object Mine extends Action {
//
//  protected def effect(player: Player, opponents: Set[Player]): Unit = ???
//
//  override val price: Int = 5
//  val isTerminal: Boolean = true
//}