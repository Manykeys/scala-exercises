package exercises02

class Functions {
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    x => y => f(x, y)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (x, y) => f(x)(y)

  def andThen[A, B, C](f: A => B)(g: B => C): A => C =
    x => g(f(x))

  def compose[A, B, C](f: B => C)(g: A => B): A => C =
    x => f(g(x))

  def const[A, B](b: B): A => B =
    _ => b

  def liftOption[A, B](f: A => B): A => Option[B] =
    x => Option(f(x))

  def chain[A](functions: List[A => A]): A => Option[A] = {
    val compositeFunction = functions.reduceOption((f, g) => f andThen g)
    value => compositeFunction.map(_(value))
  }

  def zip[A, B, C](f: A => B, g: A => C): A => (B, C) =
    x => (f(x), g(x))

  def unzip[A, B, C](f: A => (B, C)): (A => B, A => C) =
    (x => f(x)._1, x => f(x)._2)
}
