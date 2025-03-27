package exercises07

import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.ExecutionContext
import scala.util.Try

object RunEC {
  def runCallback[T](task: => T)(cb: Try[T] => Unit)(ec: ExecutionContext): Unit = {
    ec.execute(() => {
      val result = Try(task)
      cb(result)
    })
  }
  def runReturn[T](task: => T)(ec: ExecutionContext): Try[T] = {
    val queue = new ArrayBlockingQueue[Try[T]](1)
    ec.execute(() => {
      queue.put(Try(task))
    })
    queue.take()
  }

}
