import scala.concurrent.Promise

object f07_promise_create extends App {

  val p: Promise[String] = Promise()

  p.success("Горошек доставлен")

  p.failure(new RuntimeException("Ваш заказ отменен"))

}
