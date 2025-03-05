import scala.language.implicitConversions

def multiply(x: Int)(implicit y: Int): Int = x * y

implicit val z: Int = 10 // должна быть неявной

multiply(3) // result: 30
multiply(4) // result: 40
