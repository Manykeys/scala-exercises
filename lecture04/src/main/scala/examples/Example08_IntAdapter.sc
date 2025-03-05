

class IntAdapter(val i: Int) {
  def isEven: Boolean = i % 2 == 0
  def isOdd: Boolean  = !isEven
}

// Создание экземпляра адаптера и использование его методов
new IntAdapter(42).isEven
new IntAdapter(42).isOdd
