
object Example {

  implicit def intToString1(x: Int): String = x.toString
  implicit def intToString2(x: Int): String = x.toString

  val x: String = 123
}
