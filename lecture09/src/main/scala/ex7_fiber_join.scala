import scala.concurrent.duration.DurationInt

object ex7_fiber_join extends App {
  def peelPotatoes: IO[String] =
    for {
      _ <- IO.sleep(5.seconds)
    } yield "Картошка почищена"

  def chopCarrot: IO[String] =
    for {
      _ <- IO.sleep(5.seconds)
    } yield "Морковка порезана"

  val task: IO[String] = for {
    fiberPotatoes <- peelPotatoes.start
    fiberCarrot     <- chopCarrot.start

    outcomePotatoes <- fiberPotatoes.join
    outcomeCarrot     <- fiberCarrot.join

    result <- outcomePotatoes match {
      case Outcome.Succeeded(io) => io
      case Outcome.Errored(e)    => IO("Порезал палец")
      case Outcome.Canceled()    => IO("Решили не добавлять морковку")
    }
  } yield result

  override def run: IO[Unit] =
    task.flatMap(result => IO(println(s"Вот оливье: $result")))
}
