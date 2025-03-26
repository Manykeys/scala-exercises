package exercises07

trait MyThreadFactory {
  def startThread(task: () => Unit): Thread
}
