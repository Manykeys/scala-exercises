package exercises03.calculator

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def calculate(expr: Expr[T]): Result[T] = ???
}
