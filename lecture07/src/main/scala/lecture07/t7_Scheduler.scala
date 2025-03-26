package lecture07

import lecture07.t1_Task.task

import java.util.concurrent.{Executors, TimeUnit}

object t7_Scheduler extends App {
  val fixedThreadPool = Executors.newScheduledThreadPool(5)


  List.range(1, 10).foreach(
    i => {
      fixedThreadPool.schedule(
        new Runnable {
          override def run(): Unit = task(i)
        },
        5, TimeUnit.SECONDS
      )
    }
  )


  Thread.sleep(1000)
}
