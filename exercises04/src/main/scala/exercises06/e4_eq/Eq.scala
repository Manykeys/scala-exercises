package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object Eq {}

object EqInstances {
  implicit val boolEq: Eq[Boolean] = new Eq[Boolean] {
    override def eqv(a: Boolean, b: Boolean): Boolean = a == b
  }

  implicit def eqOption[A](implicit ev: Eq[A]): Eq[Option[A]] = new Eq[Option[A]] {
    override def eqv(a: Option[A], b: Option[A]): Boolean = (a, b) match {
      case (Some(x), Some(y)) => ev.eqv(x, y)
      case (None, None)       => true
      case _                  => false
    }
  }

  implicit def eqList[A](implicit ev: Eq[A]): Eq[List[A]] = new Eq[List[A]] {
    override def eqv(a: List[A], b: List[A]): Boolean =
      a.length == b.length && a.zip(b).forall { case (x, y) => ev.eqv(x, y) }
  }
  implicit val intEq: Eq[Int] = new Eq[Int] {
    override def eqv(a: Int, b: Int): Boolean = a == b
  }
}

object EqSyntax {
  implicit class EqOps[A](private val m: A) extends AnyVal {
    def eqv(b: A)(implicit eq: Eq[A]) = eq.eqv(m, b)
    def ===(b: A)(implicit eq: Eq[A]) = eq.eqv(m, b)
    def !==(b: A)(implicit eq: Eq[A]) = !eq.eqv(m, b)
  }
}

object Examples {
  import EqInstances._
  import EqSyntax._

//  1 eqv 1 // возвращает true
//  1 === 2 // возвращает false
//  1 !== 2 // возвращает true
//  // 1 === "some-string" // не компилируется
//  // 1 !== Some(2) // не компилируется
//  List(true) === List(true) // возвращает true
}
