package Components

class Supply(var cardToQty: Map[Card, Int]) {

  def getMap: Map[Card, Int] = cardToQty

  def setMap(map: Map[Card, Int]): Unit = cardToQty = map

  def remove(card: Card): Unit = setMap(cardToQty.removed(card: Card))

  def put(card: Card, qty: Int): Unit = setMap(cardToQty ++ Map(card -> qty))

  def size: Int = cardToQty.size

  def take(card: Card): Unit = cardToQty.get(card) match {
    case Some(value) =>
      if (value == 1) remove(card)
      else put(card, value -1)
    case None => throw new UnsupportedOperationException("Tried to take a card not in inventory")
  }

  def contains(card: Card): Boolean = cardToQty.contains(card)

  def provinces: Int = cardToQty.get(Province).getOrElse(0)
}
