package Components

sealed trait Treasure extends Card {

  val purchasingPower: Int
}

case object Copper extends Treasure {
  override val purchasingPower: Int = 1
  override val price: Int = 0
  val name: String = "Copper"
}

case object Silver extends Treasure {
  override val purchasingPower: Int = 2
  override val price: Int = 3
  override val name: String = "Silver"
}

case object Gold extends Treasure {
  override val purchasingPower: Int = 3
  override val price: Int = 6
  override val name: String = "Gold"
}