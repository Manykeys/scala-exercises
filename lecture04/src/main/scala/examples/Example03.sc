import scala.language.implicitConversions

trait Currency

object Currency {
  case class Dollar(amount: Double) extends Currency
  case class Euro(amount: Double)   extends Currency

  implicit def euroToDollar(euro: Euro): Dollar =
    Dollar(euro.amount * 1.13)
}

val dollar: Currency.Dollar = Currency.Euro(100) // euroToDollar
