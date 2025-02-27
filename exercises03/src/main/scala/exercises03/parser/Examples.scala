package exercises03.parser
import exercises03.either.EitherCombinators.Either.fromOption
import exercises03.either.EitherCombinators._
import exercises03.parser.Error.{Banned, InvalidBanned, InvalidId, InvalidPassport}

object Examples {

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None
    * если rawUser.id не парсится в Long то функция должна вернуть None
    * rawUser.banned не "false", вернуть None
    * используйте for-comprehension
    */
  def transformToOption(rawUser: RawUser): Option[User] = transformToEither(rawUser).toOption

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть Left(InvalidName)
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть Left(InvalidPassport)
    * если rawUser.id не парсится в Long то функция должна вернуть Left(InvalidId)
    * если rawUser.banned не "true" или "false" - вернуть Left(InvalidBanned). Если "true" - вернуть Left(Banned).
    * у ошибок есть приоритет:
    * 1. InvalidBanned
    * 2. Banned
    * 2. InvalidId
    * 3. InvalidName
    * 4. InvalidPassport
    * используйте for-comprehension
    * но для того, чтобы for-comprehension заработал надо реализовать map и flatMap в Either
    */
  private def parseBanned(banned: String): Either[Error, Unit] =
    banned.toBooleanOption
      .map {
        case true  => Left(Banned)
        case false => Right(())
      }
      .getOrElse(Left(InvalidBanned))

  private def parseId(id: String): Either[Error, Long] =
    id.toLongOption.map(Right(_)).getOrElse(Left(InvalidId))

  private def parsePassport(passport: String): Either[Error, Passport] = {
    val pattern = raw"(\d{4}) (\d{6})".r
    passport match {
      case pattern(ser, num) =>
        for {
          serL <- fromOption(ser.toLongOption)(InvalidPassport)
          numL <- fromOption(num.toLongOption)(InvalidPassport)
        } yield Passport(serL, numL)
      case _ => Left(InvalidPassport)
    }
  }

  def transformToEither(rawUser: RawUser): Either[Error, User] = {
    for {
      _          <- parseBanned(rawUser.banned)
      id         <- parseId(rawUser.id)
      firstName  <- fromOption(rawUser.firstName)(Error.InvalidName)
      secondName <- fromOption(rawUser.secondName)(Error.InvalidName)
      passport   <- parsePassport(rawUser.passport)
    } yield User(
      id = id,
      userName = UserName(firstName = firstName, secondName = secondName, thirdName = rawUser.thirdName),
      passport = passport
    )
  }
}
