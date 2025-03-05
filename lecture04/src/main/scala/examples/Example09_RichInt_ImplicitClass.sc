

implicit class RichInt(private val i: Int) extends AnyVal {
  def isEven: Boolean = i % 2 == 0
  def isOdd: Boolean = !isEven
}

// Вызов методов имплиситного класса
42.isEven
42.isOdd
