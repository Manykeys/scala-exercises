package exercises00

object Application extends App {
  def hello(name: String): String = "Hello " + name

  println(
    hello("world")
  )
}
