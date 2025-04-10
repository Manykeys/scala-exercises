import scala.concurrent.{ExecutionContext, Future, Promise}

object f08_promise_iso extends App {
  val ec = ExecutionContext.global

  def deliverGorokhCallback(cb: String => Unit): Unit = {
    val gorokh = "***"
    Thread.sleep(2000)
    ec.execute(() => cb(gorokh))
  }

  def deliveryFuture: Future[String] = {
    val p: Promise[String] = Promise()

    deliverGorokhCallback(gorokh => p.success(gorokh))

    p.future
  }

  println(deliveryFuture)

}
