package Components

import Action.Action._
import Action.Action

import scala.util.Random

trait Card {
  val name: String
  val price: Int

  override def toString: String = name
}

object Card {

  val AllBaseCards: Set[Card] = Set(Copper, Silver, Gold, Estate, Duchy, Province,
    Village, Smithy, Mine, Moat, Market, Merchant, Militia, Cellar, Workshop, Remodel)
}

sealed trait CardType

object CardType {
  val All: Vector[CardType] = Vector(ActionCard, TreasureCard, LandCard)
}

case object ActionCard extends CardType

case object TreasureCard extends CardType

case object LandCard extends CardType

case class Deck(initialCards: Vector[Card] = Vector.empty) {

  private var cards: Vector[Card] = initialCards

  def setCards(cards: Vector[Card]): Deck = {
    this.cards = cards
    this
  }

  def getCards: Vector[Card] = cards

  /**
   * Returns a Deck without one instance of the specified cards
   */
  def remove(card: Card): Deck = {
    val withoutCard = cards.filterNot(_ == card)
    val withCard = cards.filter(_ == card)
    if (withCard.isEmpty) {
      println("ss")
    }
    setCards(withoutCard ++ withCard.tail)
  }

  /**
   * Returns a Deck with the new card
   */
  def add(card: Card): Deck = setCards(cards :+ card)

  def size: Int = cards.size

  def top: Card = cards.head

  def removeTop: Deck = setCards(cards.tail)

  def empty: Unit = cards = Vector.empty

  def shuffle: Unit = cards = Random.shuffle(cards)

  def contains(card: Card): Boolean = cards.contains(card)

  def removeType(card: Card): Deck = card match {
      case _: Action => setCards(cards.filterNot(_.isInstanceOf[Action]))
      case _: Land => setCards(cards.filterNot(_.isInstanceOf[Land]))
      case _: Treasure => setCards(cards.filterNot(_.isInstanceOf[Treasure]))
    }

  def find(pred: Card => Boolean): Option[Card] = cards.find(pred)

  def ++(deck: Deck): Deck = Deck(this.cards ++ deck.getCards)
}