package exercises03.either

object EitherCombinators {

  // Необходимо самостоятельно написать методы map и flatMap
  sealed trait Either[+A, +B] {
    def map[T](either: B => T): Either[A, T]
    def flatMap[AA >: A, T](either: B => Either[AA, T]): Either[AA, T]
    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C]
    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C]
    def toOption: Option[B]
  }

  case class Left[+A, +B](get: A) extends Either[A, B] {
    override def map[T](either: B => T): Either[A, T] = Left(get)

    override def flatMap[AA >: A, T](either: B => Either[AA, T]): Either[AA, T] = Left(get)

    override def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = other match {
      case Left(_) => Left(get)
      case _       => other
    }

    override def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] = Left(get)

    override def toOption: Option[B] = None
  }

  case class Right[+A, +B](get: B) extends Either[A, B] {
    override def map[T](either: B => T): Either[A, T] = Right(either(get))

    override def flatMap[AA >: A, T](either: B => Either[AA, T]): Either[AA, T] = either(get)

    override def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = Right(get)

    override def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] = other match {
      case Left(v)  => Left(v)
      case Right(a) => Right(f(get, a))
    }

    override def toOption: Some[B] = Some(get)
  }

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] =
      option.map(Right.apply).getOrElse(Left(a))

    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      list.foldLeft[Either[E, List[B]]](Right[E, List[B]](List.empty[B]))((x, y) => x.map2(f(y))((a, b) => a :+ b))

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] =
      traverse(list) {
        case Right(v) => Right(v)
        case Left(e)  => Left(e)
      }
  }
}
