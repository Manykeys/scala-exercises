package exercises06.ex01

import exercises06.data.NonEmptyList
import exercises06.typeclasses._

object Exercise01 {
  object Syntax {
    implicit class SemiOps[A](private val a: A) extends AnyVal {
      def |+|(b: A)(implicit semigroup: Semigroup[A]): A         = semigroup.combine(a, b)
      def pure[F[_]](implicit applicative: Applicative[F]): F[A] = applicative.pure(a)
    }

    implicit class MonOps[A, F[_]](private val a: F[A]) extends AnyVal {
      def combineAll(implicit foldable: Foldable[F], monoid: Monoid[A]): A =
        foldable.foldLeft(a, monoid.empty)((acc, x) => monoid.combine(acc, x))
      def foldLeft(base: A)(f: (A, A) => A)(implicit foldable: Foldable[F]): A = foldable.foldLeft(a, base)(f)
    }

    implicit class AppOps[A, F[_]](val a: F[A]) extends AnyVal {
      def aproduct[B](b: F[B])(implicit applicative: Applicative[F]): F[(A, B)] = applicative.product(a, b)
    }

    implicit class TraversOps[A, F[_]](val a: F[A]) extends AnyVal {
      def traverse[G[_]: Applicative, B](f: A => G[B])(implicit traverse: Traverse[F]): G[F[B]] =
        traverse.traverse(a)(f)
    }

    implicit class FunctorOps[A, F[_]](val a: F[A]) extends AnyVal {
      def map[B](f: A => B)(implicit functor: Functor[F]): F[B] = functor.map(a)(f)
    }
  }
  object Instances {
    import Syntax._
    implicit val strMonoid: Monoid[String] = new Monoid[String] {
      override def empty: String = ""

      override def combine(x: String, y: String): String = x + y
    }

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0

      override def combine(x: Int, y: Int): Int = x + y
    }

    implicit val listInstances: Traverse[List] with Applicative[List] = new Traverse[List] with Applicative[List] {
      override def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
        fa.foldLeft(List.empty[B].pure[G])((accF, next) =>
          accF.aproduct(f(next)).map {
            case (acc, next) => acc.appended(next)
          }
        )

      override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = fa.zip(ff).map(x => x._2(x._1))

      override def pure[A](x: A): List[A] = List(x)

      override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }

    implicit val optionInstances: Traverse[Option] with Applicative[Option] =
      new Traverse[Option] with Applicative[Option] {
        override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
          fa.fold(Applicative[G].pure(None: Option[B]))(a => f(a).map(Some(_)))
        }

        override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

        override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = fa.zip(ff).map(x => x._2(x._1))

        override def pure[A](x: A): Option[A] = Option(x)

        override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa.fold(b)(f(b, _))
      }

    implicit val nelInstances: Traverse[NonEmptyList] with Applicative[NonEmptyList] =
      new Traverse[NonEmptyList] with Applicative[NonEmptyList] {
        override def traverse[G[_]: Applicative, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] = {
          val head = f(fa.head)

          val tail = fa.tail.foldLeft(Applicative[G].pure(List.empty[B]))((accF, next) =>
            accF.aproduct(f(next)).map {
              case (acc, next) => acc.appended(next)
            }
          )
          head.aproduct(tail).map {
            case (h, t) => NonEmptyList(h, t)
          }
        }
        override def ap[A, B](ff: NonEmptyList[A => B])(fa: NonEmptyList[A]): NonEmptyList[B] =
          NonEmptyList(
            ff.head(fa.head),
            fa.tail
              .zip(ff.tail)
              .map(x => x._2(x._1))
          )

        override def pure[A](x: A): NonEmptyList[A] = NonEmptyList(x)

        override def foldLeft[A, B](fa: NonEmptyList[A], b: B)(f: (B, A) => B): B =
          fa.tail.foldLeft(f(b, fa.head))((x, y) => f(x, y))

        override def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] =
          NonEmptyList(head = f(fa.head), tail = fa.tail.map(f))
      }

    implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      override def empty: List[A] = List.empty[A]

      override def combine(x: List[A], y: List[A]): List[A] = x ++ y
    }

  }
}
