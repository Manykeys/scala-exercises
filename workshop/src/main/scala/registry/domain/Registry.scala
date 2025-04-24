package registry.domain

import cats._
import cats.effect.Sync
import cats.syntax.all._
import registry.domain.model.User
import registry.domain.service.{ApplicationAlg, TrustworthinessAlg, UserAlg}

import scala.concurrent.duration._
import scala.util.control.NoStackTrace

trait Registry[F[_]] {
  def signUp(user: User): F[Unit]
}

object Registry {
  sealed trait Error extends Exception with NoStackTrace

  object Error {
    case object UserAlreadyExists            extends Error
    case object UserApplicationAlreadyExists extends Error
  }

  //TODO: Реализовать бизнес логику
  def build[F[_]: MonadThrow: Sync](
      userAlg: UserAlg[F],
      appAlg: ApplicationAlg[F],
      trustworthinessAlg: TrustworthinessAlg[F]
  ): Registry[F] = new Registry[F] {
    override def signUp(user: User): F[Unit] =
      for {

        maybeUser <- userAlg.getBy(user.passport)
        _ <- maybeUser.fold(Sync[F].unit) { _ =>
          MonadThrow[F].raiseError(Error.UserAlreadyExists)
        }

        maybeId <- appAlg.getApplicationBy(user)
        _ <- maybeId.fold(Sync[F].unit) { _ =>
          MonadThrow[F].raiseError(Error.UserApplicationAlreadyExists)
        }

        applicationId <- trustworthinessAlg.check(user)
        _             <- appAlg.persist(applicationId, user, 1.minute)
      } yield ()
  }
}
