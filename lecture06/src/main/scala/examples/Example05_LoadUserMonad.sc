import examples.typeclasses.Monad
import examples.typeclasses.MonadInstances._
import examples.typeclasses.MonadSyntax._

import scala.util.Try

object Service {

  case class User(id: String, name: String, surname: String)

  def idByTag[F[_]: Monad](tag: String): F[String] =
    s"$tag-id".pure[F]

  def nameById[F[_]: Monad](id: String): F[String] =
    id match {
      case "123-id" => "Alice".pure[F]
      case "456-id" => "Bob".pure[F]
      case _        => "Carol".pure[F]
    }

  def surnameById[F[_]: Monad](id: String): F[String] =
    id match {
      case "123-id" => "Johnson".pure[F]
      case "657-id" => "Anderson".pure[F]
      case _        => "Smith".pure[F]
    }

  def loadUser[F[_]: Monad](tag: String): F[User] =
    for {
      id      <- idByTag[F](tag)
      name    <- nameById[F](id)
      surname <- surnameById[F](id)
    } yield User(id, name, surname)
}

Service.loadUser[Option]("123")
Service.loadUser[Try]("456")
Service.loadUser[Either[String, *]]("444")

