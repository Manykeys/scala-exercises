import scala.language.implicitConversions

trait Area {
  def area: Double
}

class Circle(radius: Double) extends Area {
  override def area: Double = math.Pi * math.pow(radius, 2)
}

class Rectangle(width: Double, length: Double) extends Area {
  override def area: Double = width * length
}

// Обобщенная функция
def areaOf(area: Area): Double = area.area

areaOf(new Circle(10))
areaOf(new Rectangle(5, 5))
