import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object f06_future_create_eval extends App {
  def chopPotatoes(): Future[String] = Future {
    Thread.sleep(3000)
    "🥔 Картошка нарезана"
  }

  val result = Await.result(chopPotatoes(), 5.seconds)

  println(result)
}
