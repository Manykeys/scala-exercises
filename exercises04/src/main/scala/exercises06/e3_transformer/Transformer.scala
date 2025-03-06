package exercises06.e3_transformer

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]

  def toEither(a: A): Either[Error, B]
}

object TransformerInstances {
  implicit val transformer: Transformer[RawUser, User] = new Transformer[RawUser, User] {

    override def toOption(a: RawUser): Option[User] = toEither(a).toOption

    override def toEither(a: RawUser): Either[Error, User] =
      for {
        userId     <- a.id.toLongOption.map(Right(_)).getOrElse(Left(InvalidId))
        firstName  <- a.firstName.map(Right(_)).getOrElse(Left(InvalidName))
        secondName <- a.secondName.map(Right(_)).getOrElse(Left(InvalidName))
      } yield User(userId, UserName(firstName, secondName, a.thirdName))
  }
}
object TransformerSyntax {
  implicit class TransformerOps[A, B](private val value: A) extends AnyVal {
    def transformToOption[B](implicit ev: Transformer[A, B]): Option[B]        = ev.toOption(value)
    def transformToEither[B](implicit ev: Transformer[A, B]): Either[Error, B] = ev.toEither(value)
  }
}

object Examples {
  import TransformerInstances._
  import TransformerSyntax._

  RawUser("1234", Some(""), Some(""), None).transformToOption[User]
  RawUser("1234", Some(""), Some(""), None).transformToEither[User]
}
