import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object f09_future_map extends App {
  implicit val ec = ExecutionContext.global

  val futa: Future[Int] = Future.successful(10)

  val futaMapped: Future[Int] =
    futa
      .map(_ * 2)
      .map(_ + 1)
      .map(_ * 2)

  val result: Int = Await.result(futaMapped, 10.seconds)

  println(result)

}
