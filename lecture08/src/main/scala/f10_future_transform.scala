import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object f10_future_transform extends App {

  val futa: Future[String] = Future.failed(new RuntimeException("Нет колбасы"))

  val res = futa.transform(
    (suc: String) => "Режем колбасу",
    (err: Throwable) => new RuntimeException("Никакого оливье сегодня :(")
  )

  println {
    Await.result(res, 10.seconds)
  }
}
