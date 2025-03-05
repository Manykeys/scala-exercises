import scala.language.implicitConversions

case class Context(requestId: String)

class Logger {
  def log(message: String)(ctx: Context): Unit = {
    println(s"[${ctx.requestId}] $message")
  }
}

val logger: Logger = new Logger()

def handle(context: Context): Unit = {
  // some action ...
  logger.log("Starting process")(context)
  // some action ...
  logger.log("Continue process...")(context)
  // some action ...
  logger.log("End process")(context)
}

handle(Context("ID"))
