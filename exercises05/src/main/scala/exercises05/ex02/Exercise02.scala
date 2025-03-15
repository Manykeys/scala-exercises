package exercises06.ex02

import exercises06.data.NonEmptyList
import exercises06.ex02.Domain._
import exercises06.ex02.Errors._
import exercises06.typeclasses._

object Exercise02 {
  type TransformationSupport[F[_]] = ApplicativeError[F, NonEmptyList[ParsingError]]

  object TransformationSupport {
    @inline
    def apply[F[_]](implicit inst: TransformationSupport[F]): TransformationSupport[F] =
      inst
  }

  private implicit class OptionOps[A](private val opt: Option[A]) extends AnyVal {
    def require[F[_]](err: => ParsingError)(implicit ts: TransformationSupport[F]): F[A] =
      opt match {
        case Some(value) => ts.pure(value)
        case None        => ts.raiseError(NonEmptyList.of(err))
      }
  }

  // Советуем воспользоваться)
  import TransformerSyntax._
  import TupleSyntax._
  import exercises06.ex01.Exercise01.Instances._
  import exercises06.ex01.Exercise01.Syntax._

  implicit def personTransformerF[F[_]: TransformationSupport]: TransformerF[F, RawPerson, Person] =
    new TransformerF[F, RawPerson, Person] {
      override def transform(from: RawPerson): F[Person] = {
        val id: F[Long] = from.id.toLongOption.require[F](InvalidPersonId(from.id))

        val name: F[String] = from.name.require[F](MissingPersonName)

        val phone: F[Phone] = Phone.parse(from.phone).require[F](InvalidPhone(from.phone))

        (id, name, phone).mapN(Person)
      }
    }

  implicit def addressBookTransformerF[F[_]: TransformationSupport]: TransformerF[F, RawAddressBook, AddressBook] =
    new TransformerF[F, RawAddressBook, AddressBook] {
      override def transform(from: RawAddressBook): F[AddressBook] = {
        val id: F[Long] = from.id.toLongOption.require[F](InvalidAddressBookId(from.id))

        val persons: F[List[Person]] = from.persons.traverse(_.transformF[F, Person])

        (id, persons).mapN(AddressBook)
      }
    }

}
