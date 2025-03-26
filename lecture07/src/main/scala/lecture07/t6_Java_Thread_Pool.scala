package lecture07

import java.util.concurrent.Executors
import lecture07.t1_Task.task

object t6_Java_Thread_Pool extends App {
  val fixedThreadPool = Executors.newFixedThreadPool(5)

  List.range(0, 10).foreach(
    i => fixedThreadPool.execute(
      () => task(i)
    )
  )


  Thread.sleep(1000)
}
