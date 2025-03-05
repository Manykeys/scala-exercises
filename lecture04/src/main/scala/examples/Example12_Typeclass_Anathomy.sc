
// тайпкласс
trait TypeClass[A] {
  def method(value: A): Unit
}

// создаем объект с методом apply
object TypeClass {
  def apply[A](implicit ev: TypeClass[A]): TypeClass[A] = ev
}

// Инстансы
object Instances {
  implicit val intInstance: TypeClass[Int] = new TypeClass[Int] {
    def method(value: Int): Unit = println(s"int instance $value")
  }

  implicit val stringInstance: TypeClass[String] = new TypeClass[String] {
    def method(value: String): Unit = println(s"string instance $value")
  }
}

// Синтаксис
object syntax {
  implicit class TypeClassOps[A](private val value: A) extends AnyVal {
    def method(implicit ev: TypeClass[A]): Unit = ev.method(value)
  }
}

// Использование тайпкласса
object SomeApp {
  // передаем инстанс тайпкласса через аргументы метода
  def someMethod1[A](arg: A)(implicit t: TypeClass[A]): Unit = {
    t.method(arg)
  }

  // передаем инстанс тайпкласса через контекст баунды
  def someMethod2[A: TypeClass](arg: A): Unit = {
    // достаем инстанс через apply
    TypeClass[A].method(arg)
  }

  // Используем синтаксис
  import syntax._
  def someMethod3[A: TypeClass](arg: A): Unit = {
    arg.method
  }
}


import Instances._

SomeApp.someMethod1(42)
SomeApp.someMethod2(42)
SomeApp.someMethod3(42)