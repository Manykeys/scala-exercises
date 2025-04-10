import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

object f11_future_transform_with extends App {
  val futa: Future[Int] = Future.failed(new RuntimeException())

  val result = futa.transform {
    case Failure("Нет колбасы") => Success("Возьмем мяско")
    case Success(value)         => Success(value)
  }

  println {
    Await.result(result, 10.seconds)
  }
}
