package exercises02

object Partial {
  def combo[I, T](functions: List[PartialFunction[I, T]]): I => Option[T] =
    input =>
      functions.collectFirst {
        case partialFunc if partialFunc.isDefinedAt(input) => partialFunc(input)
      }
}
