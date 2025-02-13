package exercises00

object Application extends App {
  def hello(name: String): String = "Hi " + name

  println(
    hello("world")
  )
}
