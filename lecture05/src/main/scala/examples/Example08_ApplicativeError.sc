import examples.typeclasses.ApplicativeError
import examples.typeclasses.ApplicativeErrorInstances._
import examples.typeclasses.ApplicativeErrorSyntax._
import examples.typeclasses.ApplicativeSyntax._

import scala.util.Try

case class Temperature(value: Double)

def getTemperature[F[_]](city: String)(implicit ae: ApplicativeError[F, Throwable]): F[Temperature] = {
  city match {
    case "Moscow"        => Temperature(-5).pure[F]
    case "Yekaterinburg" => Temperature(-10).pure[F]
    case "Sochi"         => Temperature(15).pure[F]
    case unknown         => ApplicativeError[F, Throwable].raiseError(new Throwable(s"City $unknown wasn't found"))
  }
}

// Try examples
getTemperature[Try]("Moscow")
getTemperature[Try]("Sochi")
getTemperature[Try]("Kaliningrad")

val allCitiesSuccess: Try[(Temperature, Temperature, Temperature)] = (
  getTemperature[Try]("Moscow"),
  getTemperature[Try]("Sochi"),
  getTemperature[Try]("Yekaterinburg")
).mapN {
  case (msk, sch, yek) => (msk, sch, yek)
}

println(allCitiesSuccess)

val allCitiesFail: Try[(Temperature, Temperature, Temperature)] = (
  getTemperature[Try]("Moscow"),
  getTemperature[Try]("Sochi"),
  getTemperature[Try]("Kaliningrad")
).mapN {
  case (msk, sch, k) => (msk, sch, k)
}

println(allCitiesFail)

// Either examples
type EitherTh[A] = Either[Throwable, A]

getTemperature[EitherTh]("Moscow")
getTemperature[EitherTh]("Sochi")
getTemperature[EitherTh]("Kaliningrad")

val allCitiesEither: Try[(Temperature, Temperature, Temperature)] = (
  getTemperature[Try]("Moscow"),
  getTemperature[Try]("Sochi"),
  getTemperature[Try]("Kaliningrad")
).mapN {
  case (msk, sch, klngrd) => (msk, sch, klngrd)
}

println(allCitiesEither)

allCitiesEither.handleErrorWith {
  error: Throwable =>
    println(s"Error: $error")
    (Temperature(0), Temperature(0), Temperature(0)).pure[Try]
}
