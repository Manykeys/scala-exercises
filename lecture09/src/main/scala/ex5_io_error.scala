object ex5_io_error extends App {
  case class NoSausageException() extends RuntimeException("Кончилась колбаса!")
  case class NoPotatoException() extends RuntimeException("Кончилась картошка!")

  // Создание
  val makeOlivier: IO[String] = IO.raiseError(new RuntimeException(NoSausageException()))

  // handleError
  val handled: IO[String] = makeOlivier.handleError(_ => "Положим мяско!")
  val handledWith: IO[String] = makeOlivier.handleErrorWith(_ => IO.println("Идем в магаз за колбасой :("))

  // recover
  val recovered: IO[String] = makeOlivier.recover {
    case _: NoSausageException => "Положим мяско!"
    case _: NoPotatoException  => "Положим батат!"
  }
  val recoveredWith: IO[String] = makeOlivier.recoverWith {
    case _: NoSausageException => IO.println("Идем в магаз за колбасой :(")
    case _: NoPotatoException  => IO.println("Идем в магаз за картошкой :(")
  }

// redeem
  val redeemed: IO[String] = makeOlivier.redeem(
    _ => "Положим мяско!",
    _ => "Положим колбаску!"
  )
  val redeemedWith: IO[String] = makeOlivier.redeemWith(
    _ => IO.println("Идем в магаз за колбасой :("),
    _ => IO.println("Режем колбаску")
  )
}
