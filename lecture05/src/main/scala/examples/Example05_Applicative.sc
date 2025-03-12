import examples.typeclasses.{Applicative, Functor}
import examples.typeclasses.ApplicativeInstances._
import examples.typeclasses.ApplicativeSyntax._
import examples.typeclasses.FunctorSyntax.FunctorOps

import scala.util.Try


// --------------
// Pure
Applicative[Option].pure(42)
Applicative[List].pure(42)

42.pure[Option] // Some(42)
42.pure[List]   // List(42)
42.pure[Try]    // Success(42)

case class Temperature(value: Double)

def getTemperature[F[_]: Applicative](city: String): F[Temperature] = {
  city match {
    case "Moscow"        => Temperature(-5).pure[F]
    case "Yekaterinburg" => Temperature(-10).pure[F]
    case "Sochi"         => Temperature(15).pure[F]
  }
}

getTemperature[Option]("Moscow")
getTemperature[Try]("Yekaterinburg")


// --------------
// Product

Applicative[Option].product(
  getTemperature[Option]("Moscow"),
  getTemperature[Option]("Sochi")
)

Applicative[Try].product(
  getTemperature[Try]("Moscow"),
  getTemperature[Try]("Sochi")
)

// ----------
// mapN
(
  getTemperature[Option]("Moscow"),
  getTemperature[Option]("Yekaterinburg"),
  getTemperature[Option]("Sochi")
).mapN { case (a, b, c) => (a, b, c) }


// Applicative Laws
val fa: Option[String] = Some("a")
val fb: Option[String] = Some("b")
val fc: Option[String] = Some("c")

def associativity[F[_]: Applicative, A, B, C](
    fa: F[A],
    fb: F[B],
    fc: F[C]
): Boolean = {
  fa.product(fb.product(fc)) == fa.product(fb).product(fc).map { case ((a, b), c) => (a, (b, c)) }
  // (a, (b, c)) ~ ((a, b), c)
}

val associativityResult: Boolean = associativity(fa, fb, fc)
println(associativityResult)

// Right identity
def rightIdentity[F[_]: Applicative, A](fa: F[A]): Boolean = {
  fa.product(().pure[F]).map(_._1) == fa
  // (a, ()) ~ (a)
}

rightIdentity(fa)


// Left identity
def leftIdentity[F[_]: Applicative, A](fa: F[A]): Boolean = {
  ().pure[F].product(fa).map(_._2) == fa
  // ((), a ~ (a)
}

leftIdentity(fa)
