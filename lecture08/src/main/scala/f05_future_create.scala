import scala.concurrent.Future

object f05_future_create extends App {

  val f1: Future[String] = Future.successful("Картошка сварена")

  val f2: Future[String] = Future.failed(new Throwable("Картошка закончилась"))
}
