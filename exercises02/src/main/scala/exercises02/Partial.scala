package exercises02

object Partial {
  def combo[I, T](functions: List[PartialFunction[I, T]]): I => Option[T] = {
    val unionFunction = functions.reduceOption((f, g) => f orElse g)
    input => unionFunction.flatMap(_.lift(input))
  }
}
