package exercises02

object Partial {
  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] = ???

}
