package exercises00

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(x + other.x, y + other.y)

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y)

  def *(scalar: Double): Vector = new Vector(x * scalar, y * scalar)

  def unary_- : Vector = new Vector(-x, -y)

  def euclideanLength: Double = math.sqrt(math.pow(x, 2) + math.pow(y, 2))

  def normalized: Vector = new Vector(x / euclideanLength, y / euclideanLength)

  override def equals(other: Any): Boolean =
    other match {
    case Vector(a, b) => x == a && y == b
    case _ => false
  }

  // Vector(x, y)
  override def toString: String = s"Vector($x, $y)"
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector =
    new Vector(length * math.cos(angle), length * math.sin(angle))

  def sum(list: List[Vector]): Vector = list.foldLeft(new Vector(0, 0))((x, y) => x + y)

  def unapply(arg: Vector): Option[(Double, Double)] = Option((arg.x, arg.y))
}
