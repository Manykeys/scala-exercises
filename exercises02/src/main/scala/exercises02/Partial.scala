package exercises02

object Partial {
  def combo[I, T](functions: List[PartialFunction[I, T]]): I => Option[T] = {
    val unionFunction = functions.reduceOption((f, g) => f orElse g)
    value =>
      unionFunction match {
        case Some(f) if f.isDefinedAt(value) => Option(f(value))
        case _                               => None
      }
  }
}
