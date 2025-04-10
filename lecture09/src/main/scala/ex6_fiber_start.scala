import scala.concurrent.duration.DurationInt

object ex6_fiber_start extends App {
  // Один повар бесконечно режет картошку с задержкой
  def cutPotatoes: IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(println("🔪 Режем картошку"))
      _ <- cutPotatoes
    } yield ()

  // Другой повар бесконечно режет морковку с задержкой
  def cutCarrots: IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(println("🔪 Режем морковку"))
      _ <- cutCarrots
    } yield ()

  val task: IO[Unit] = for {
    fiber1 <- cutPotatoes.start // Запускаем файбер 1
    fiber2 <- cutCarrots.start // Запускаем файбер 2
  } yield ()

  task.unsafeRunSync() // Запуск основного файбера. Основной файбер запустит файберы 1 и 2.
  Thread.sleep(10000)
}
