package writer.app

import typeclasses.Semigroup

object domain {
  case class Good(price: BigDecimal)
  case class Wallet(amount: BigDecimal)
  case class Transaction(price: BigDecimal)
  object Transaction {
    implicit val semigroup: Semigroup[Transaction] = new Semigroup[Transaction] {
      override def combine(x: Transaction, y: Transaction): Transaction = Transaction(x.price + y.price)
    }
  }
}
