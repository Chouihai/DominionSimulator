package Components

sealed trait Land extends Card {
  val victoryPoints: Int
}

case object Estate extends Land {
  override val victoryPoints: Int = 1
  override val price: Int = 2
  val name: String = "Estate"
}

case object Duchy extends Land {
  override val victoryPoints: Int = 3
  override val price: Int = 5
  val name: String = "Duchy"
}

case object Province extends Land {
  override val victoryPoints: Int = 6
  override val price: Int = 8
  val name: String = "Province"
}