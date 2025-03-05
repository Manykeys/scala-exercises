import scala.language.implicitConversions

object ExternalImplicits {
  implicit def intToString(x: Int): String = x.toString
}

//import ExternalImplicits.intToString
// ИЛИ импортируем все
import ExternalImplicits._

val x: String = 123
