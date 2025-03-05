import scala.language.implicitConversions

case class Context(requestId: String)

class Logger {
  def log(message: String)(implicit ctx: Context): Unit = {
    println(s"[${ctx.requestId}] $message")
  }
}

val logger: Logger = new Logger()

def handle(implicit context: Context): Unit = {
  // some action ...
  logger.log("Starting process")
  // some action ...
  logger.log("Continue process...")
  // some action ...
  logger.log("End process")
}

println(handle(Context("ID")))
